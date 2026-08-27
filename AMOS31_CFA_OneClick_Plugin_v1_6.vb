Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Diagnostics
Imports System.Globalization
Imports System.IO
Imports System.Text
Imports Microsoft.VisualBasic
Imports Amos
Imports AmosEngineLib
Imports AmosEngineLib.AmosEngine.TMatrixID

<System.ComponentModel.Composition.Export(GetType(Amos.IPlugin))>
Public Class CustomCode
    Implements IPlugin

    Private Class ConstructInfo
        Public Name As String
        Public DisplayName As String
        Public Items As New List(Of String)()
        Public Loadings As New Dictionary(Of String, Double)(StringComparer.OrdinalIgnoreCase)
        Public Alpha As Double = Double.NaN
        Public Ave As Double = Double.NaN
    End Class

    Public Function Name() As String Implements IPlugin.Name
        Return "CFA One-Click Report"
    End Function

    Public Function Description() As String Implements IPlugin.Description
        Return "Runs a first-order single-group CFA report: fit indices, standardized loadings, Cronbach alpha, AVE, Fornell-Larcker and HTMT. (v1.6)"
    End Function

    Public Function Mainsub() As Integer Implements IPlugin.Mainsub
        Dim sem As AmosEngineLib.AmosEngine = Nothing

        Try
            Dim constructs As List(Of ConstructInfo) = DiscoverConstructs()
            If constructs.Count = 0 Then
                MsgBox("No first-order CFA constructs were found. Each construct must be an unobserved variable with arrows to at least two observed indicators.", MsgBoxStyle.Exclamation, Name())
                Return 1
            End If

            Dim validationMessage As String = ValidateConstructMembership(constructs)
            If validationMessage <> "" Then
                MsgBox(validationMessage, MsgBoxStyle.Exclamation, Name())
                Return 1
            End If

            'Refresh normal Amos output. Standardized estimates are turned on because
            'Table 3 (Fornell-Larcker) must use AMOS's standardized latent-factor
            'correlations from the Correlations output table.
            Try
                Pd.GetCheckBox("AnalysisPropertiesForm", "StandardizedCheck").Checked = True
            Catch
                'If a future AMOS build renames the checkbox, continue; the user's
                'existing output settings may already include standardized estimates.
            End Try
            Dim amosOutputPath As String = Amos.Pd.ProjectName & ".AmosOutput"
            Dim previousOutputWriteTicks As Long = 0
            Dim previousOutputLength As Long = -1
            Try
                If File.Exists(amosOutputPath) Then
                    previousOutputWriteTicks = File.GetLastWriteTimeUtc(amosOutputPath).Ticks
                    previousOutputLength = New FileInfo(amosOutputPath).Length
                End If
            Catch
            End Try

            Pd.AnalyzeCalculateEstimates()

            sem = New AmosEngineLib.AmosEngine()
            sem.NeedEstimates(SampleCovariances)
            sem.NeedEstimates(SampleCorrelations)
            sem.NeedEstimates(StandardizedDirectEffects)

            'Copy the currently drawn model and its data-file settings into the engine.
            Pd.SpecifyModel(sem)

            Dim fitStatus As Integer = sem.FitModel()
            If fitStatus <> 0 Then
                MsgBox("AMOS could not obtain a solution for the current model. Run Calculate Estimates normally and resolve the model/identification problem first.", MsgBoxStyle.Critical, Name())
                Return 1
            End If

            Dim sampleCov(,) As Double = Nothing
            Dim sampleCor(,) As Double = Nothing
            Dim stdDirect(,) As Double = Nothing

            Dim covRows() As String = Nothing
            Dim covCols() As String = Nothing
            Dim corRows() As String = Nothing
            Dim corCols() As String = Nothing
            Dim loadingRows() As String = Nothing
            Dim loadingCols() As String = Nothing

            sem.GetEstimates(SampleCovariances, sampleCov)
            sem.RowNames(SampleCovariances, covRows)
            sem.ColumnNames(SampleCovariances, covCols)

            sem.GetEstimates(SampleCorrelations, sampleCor)
            sem.RowNames(SampleCorrelations, corRows)
            sem.ColumnNames(SampleCorrelations, corCols)

            sem.GetEstimates(StandardizedDirectEffects, stdDirect)
            sem.RowNames(StandardizedDirectEffects, loadingRows)
            sem.ColumnNames(StandardizedDirectEffects, loadingCols)

            'Calculate loadings, reliability and AVE construct by construct.
            For Each c As ConstructInfo In constructs
                c.Loadings.Clear()
                Dim sumSqLoadings As Double = 0.0
                Dim validLoadings As Integer = 0

                For Each item As String In c.Items
                    Dim loading As Double = MatrixValue(stdDirect, loadingRows, loadingCols, item, c.Name)
                    If Double.IsNaN(loading) Then
                        'Defensive fallback in case a particular AMOS build exposes the matrix transposed.
                        loading = MatrixValue(stdDirect, loadingRows, loadingCols, c.Name, item)
                    End If
                    c.Loadings(item) = loading
                    If Not Double.IsNaN(loading) Then
                        sumSqLoadings += loading * loading
                        validLoadings += 1
                    End If
                Next

                If validLoadings = c.Items.Count AndAlso validLoadings > 0 Then
                    c.Ave = sumSqLoadings / CDbl(validLoadings)
                Else
                    c.Ave = Double.NaN
                End If

                c.Alpha = CronbachAlpha(c.Items, sampleCov, covRows, covCols)
            Next

            Dim amosOutputText As String = ReadAmosOutputText(amosOutputPath, previousOutputWriteTicks, previousOutputLength)
            Dim cfi As Double = ReadCfiFromAmosOutput(amosOutputText, sem.Cmin, sem.Df)
            Dim latentCorrelations As Dictionary(Of String, Double) = ReadLatentCorrelationsFromAmosOutput(amosOutputText, constructs)
            Dim reportPath As String = BuildReportPath()

            WriteHtmlReport(reportPath, constructs, sem, cfi,
                            sampleCov, covRows, covCols,
                            sampleCor, corRows, corCols,
                            latentCorrelations)

            Dim psi As New ProcessStartInfo(reportPath)
            psi.UseShellExecute = True
            Process.Start(psi)
            Return 0

        Catch ex As Exception
            MsgBox("CFA One-Click Report stopped because of an error:" & vbCrLf & vbCrLf & ex.Message,
                   MsgBoxStyle.Critical, Name())
            Return 1
        Finally
            If sem IsNot Nothing Then
                Try
                    sem.Dispose()
                Catch
                End Try
            End If
        End Try
    End Function

    '============================
    ' Model discovery / validation
    '============================

    Private Function DiscoverConstructs() As List(Of ConstructInfo)
        Dim outgoing As New Dictionary(Of String, List(Of String))(StringComparer.OrdinalIgnoreCase)
        Dim factorElements As New Dictionary(Of String, PDElement)(StringComparer.OrdinalIgnoreCase)
        Dim element As PDElement

        For Each element In Pd.PDElements
            If element.IsPath Then
                If element.Variable1 IsNot Nothing AndAlso element.Variable2 IsNot Nothing Then
                    If element.Variable1.IsUnobservedVariable AndAlso element.Variable2.IsObservedVariable Then
                        Dim factorName As String = element.Variable1.NameOrCaption
                        Dim itemName As String = element.Variable2.NameOrCaption

                        If Not outgoing.ContainsKey(factorName) Then
                            outgoing(factorName) = New List(Of String)()
                            factorElements(factorName) = element.Variable1
                        End If
                        If Not ContainsIgnoreCase(outgoing(factorName), itemName) Then
                            outgoing(factorName).Add(itemName)
                        End If
                    End If
                End If
            End If
        Next

        Dim result As New List(Of ConstructInfo)()
        For Each kvp As KeyValuePair(Of String, List(Of String)) In outgoing
            'Residual/error terms normally have one outgoing arrow. Requiring >=2 observed
            'children separates first-order constructs from residuals and supports alpha/HTMT.
            If kvp.Value.Count >= 2 Then
                Dim c As New ConstructInfo()
                c.Name = kvp.Key
                For Each item As String In kvp.Value
                    c.Items.Add(item)
                Next

                Dim factorElement As PDElement = Nothing
                If factorElements.ContainsKey(kvp.Key) Then factorElement = factorElements(kvp.Key)
                c.DisplayName = ResolveConstructDisplayName(factorElement, c.Name, c.Items)
                result.Add(c)
            End If
        Next

        'Never allow one stale/duplicate AMOS label to overwrite every construct name.
        'If two or more constructs resolve to the same display text, fall back only for
        'those duplicates to a unique item stem (when available), otherwise to F1/F2/etc.
        EnsureUniqueDisplayNames(result)

        result.Sort(Function(a As ConstructInfo, b As ConstructInfo) String.Compare(a.DisplayName, b.DisplayName, StringComparison.OrdinalIgnoreCase))
        Return result
    End Function

    Private Function ResolveConstructDisplayName(ByVal factorElement As PDElement,
                                                 ByVal internalName As String,
                                                 ByVal items As List(Of String)) As String
        'IMPORTANT: do not select variables and read ObjectPropertiesForm here.
        'In AMOS 31 that form can keep the text of the previously selected construct,
        'which is how every heading can accidentally become the same name (for example AT).
        'PDElement.LongLabel is the variable label attached to this exact ellipse.
        Dim label As String = ReadLongLabel(factorElement)
        If label <> "" Then Return label

        'If the latent variable itself has a meaningful name, use it.
        If Not IsGenericFactorName(internalName) Then Return internalName

        'For auto-named factors (F1, F2, ...), use the indicator stem only when it is
        'available. EnsureUniqueDisplayNames() below prevents duplicate stems.
        Dim inferred As String = InferConstructNameFromItems(items)
        If inferred <> "" Then Return inferred

        Return internalName
    End Function

    Private Function ReadLongLabel(ByVal factorElement As PDElement) As String
        If factorElement Is Nothing Then Return ""

        Try
            Dim label As String = factorElement.LongLabel
            If label Is Nothing Then Return ""
            Return label.Trim()
        Catch
            'LongLabel has existed in the PDElement API for many AMOS releases, but
            'retain a safe fallback to the internal variable name if unavailable.
            Return ""
        End Try
    End Function

    Private Sub EnsureUniqueDisplayNames(ByVal constructs As List(Of ConstructInfo))
        If constructs Is Nothing Then Return

        Dim counts As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase)
        For Each c As ConstructInfo In constructs
            Dim key As String = If(c.DisplayName, "").Trim()
            If key = "" Then key = c.Name
            If counts.ContainsKey(key) Then
                counts(key) += 1
            Else
                counts(key) = 1
            End If
        Next

        'First repair duplicated labels using construct-specific indicator stems.
        For Each c As ConstructInfo In constructs
            Dim current As String = If(c.DisplayName, "").Trim()
            If current = "" OrElse (counts.ContainsKey(current) AndAlso counts(current) > 1) Then
                Dim inferred As String = InferConstructNameFromItems(c.Items)
                If inferred <> "" Then c.DisplayName = inferred Else c.DisplayName = c.Name
            End If
        Next

        'Second pass: if indicator stems are also duplicated, guarantee uniqueness by
        'falling back to the AMOS internal factor name for the duplicated constructs.
        counts.Clear()
        For Each c As ConstructInfo In constructs
            Dim key As String = If(c.DisplayName, "").Trim()
            If counts.ContainsKey(key) Then
                counts(key) += 1
            Else
                counts(key) = 1
            End If
        Next

        For Each c As ConstructInfo In constructs
            Dim key As String = If(c.DisplayName, "").Trim()
            If key = "" OrElse (counts.ContainsKey(key) AndAlso counts(key) > 1) Then
                c.DisplayName = c.Name
            End If
        Next
    End Sub

    Private Function IsGenericFactorName(ByVal name As String) As Boolean
        If name Is Nothing Then Return False
        Dim s As String = name.Trim()
        If s.Length < 2 Then Return False
        If Char.ToUpperInvariant(s.Chars(0)) <> "F"c Then Return False

        For i As Integer = 1 To s.Length - 1
            If Not Char.IsDigit(s.Chars(i)) Then Return False
        Next
        Return True
    End Function

    Private Function InferConstructNameFromItems(ByVal items As List(Of String)) As String
        If items Is Nothing OrElse items.Count = 0 Then Return ""

        Dim stem As String = Nothing
        For Each item As String In items
            Dim current As String = IndicatorStem(item)
            If current = "" Then Return ""

            If stem Is Nothing Then
                stem = current
            ElseIf Not stem.Equals(current, StringComparison.OrdinalIgnoreCase) Then
                'Use their common prefix only when it remains meaningful.
                Dim maxLen As Integer = Math.Min(stem.Length, current.Length)
                Dim k As Integer = 0
                While k < maxLen AndAlso Char.ToUpperInvariant(stem.Chars(k)) = Char.ToUpperInvariant(current.Chars(k))
                    k += 1
                End While
                stem = stem.Substring(0, k).TrimEnd("_"c, "-"c, " "c)
                If stem.Length < 2 Then Return ""
            End If
        Next

        If stem Is Nothing Then Return ""
        Return stem.Trim()
    End Function

    Private Function IndicatorStem(ByVal item As String) As String
        If item Is Nothing Then Return ""
        Dim s As String = item.Trim()
        If s = "" Then Return ""

        Dim i As Integer = s.Length - 1
        While i >= 0 AndAlso Char.IsDigit(s.Chars(i))
            i -= 1
        End While
        While i >= 0 AndAlso (s.Chars(i) = "_"c OrElse s.Chars(i) = "-"c OrElse Char.IsWhiteSpace(s.Chars(i)))
            i -= 1
        End While

        If i < 0 Then Return ""
        Return s.Substring(0, i + 1)
    End Function

    Private Function ValidateConstructMembership(ByVal constructs As List(Of ConstructInfo)) As String
        Dim owner As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
        Dim problems As New List(Of String)()

        For Each c As ConstructInfo In constructs
            If c.Items.Count < 2 Then
                problems.Add("Construct '" & c.Name & "' has fewer than two indicators.")
            End If

            For Each item As String In c.Items
                If owner.ContainsKey(item) AndAlso Not owner(item).Equals(c.Name, StringComparison.OrdinalIgnoreCase) Then
                    problems.Add("Indicator '" & item & "' cross-loads on both '" & owner(item) & "' and '" & c.Name & "'.")
                Else
                    owner(item) = c.Name
                End If
            Next
        Next

        'First version intentionally excludes directed paths between latent variables (higher-order
        'or structural relations), because the requested reliability/HTMT table assumes first-order CFA.
        Dim element As PDElement
        For Each element In Pd.PDElements
            If element.IsPath AndAlso element.Variable1 IsNot Nothing AndAlso element.Variable2 IsNot Nothing Then
                If element.Variable1.IsUnobservedVariable AndAlso element.Variable2.IsUnobservedVariable Then
                    Dim fromName As String = element.Variable1.NameOrCaption
                    Dim toName As String = element.Variable2.NameOrCaption
                    If HasConstruct(constructs, fromName) OrElse HasConstruct(constructs, toName) Then
                        problems.Add("Directed latent-to-latent path detected ('" & fromName & "' -> '" & toName & "'). Version 1 is for first-order measurement models only.")
                    End If
                End If
            End If
        Next

        If problems.Count = 0 Then Return ""

        Dim sb As New StringBuilder()
        sb.AppendLine("The CFA report was not produced because this Version 1 plugin expects a single-group, first-order reflective CFA.")
        sb.AppendLine()
        For Each p As String In problems
            sb.AppendLine("• " & p)
        Next
        Return sb.ToString()
    End Function

    Private Function HasConstruct(ByVal constructs As List(Of ConstructInfo), ByVal name As String) As Boolean
        For Each c As ConstructInfo In constructs
            If c.Name.Equals(name, StringComparison.OrdinalIgnoreCase) Then Return True
        Next
        Return False
    End Function

    Private Function ContainsIgnoreCase(ByVal values As List(Of String), ByVal target As String) As Boolean
        For Each value As String In values
            If value.Equals(target, StringComparison.OrdinalIgnoreCase) Then Return True
        Next
        Return False
    End Function

    '============================
    ' Statistics
    '============================

    Private Function CronbachAlpha(ByVal items As List(Of String),
                                   ByVal cov(,) As Double,
                                   ByVal rowNames() As String,
                                   ByVal colNames() As String) As Double
        Dim k As Integer = items.Count
        If k < 2 Then Return Double.NaN

        Dim sumVariances As Double = 0.0
        Dim varianceOfSum As Double = 0.0

        For i As Integer = 0 To k - 1
            Dim v As Double = MatrixValue(cov, rowNames, colNames, items(i), items(i))
            If Double.IsNaN(v) Then Return Double.NaN
            sumVariances += v
            varianceOfSum += v
        Next

        For i As Integer = 0 To k - 2
            For j As Integer = i + 1 To k - 1
                Dim cv As Double = MatrixValue(cov, rowNames, colNames, items(i), items(j))
                If Double.IsNaN(cv) Then Return Double.NaN
                varianceOfSum += 2.0 * cv
            Next
        Next

        If varianceOfSum <= 0.0 Then Return Double.NaN
        Return (CDbl(k) / CDbl(k - 1)) * (1.0 - (sumVariances / varianceOfSum))
    End Function

    Private Function Htmt(ByVal a As ConstructInfo,
                          ByVal b As ConstructInfo,
                          ByVal cor(,) As Double,
                          ByVal rowNames() As String,
                          ByVal colNames() As String) As Double
        Dim crossMean As Double = MeanAbsoluteCrossCorrelations(a.Items, b.Items, cor, rowNames, colNames)
        Dim withinA As Double = MeanAbsoluteWithinCorrelations(a.Items, cor, rowNames, colNames)
        Dim withinB As Double = MeanAbsoluteWithinCorrelations(b.Items, cor, rowNames, colNames)

        If Double.IsNaN(crossMean) OrElse Double.IsNaN(withinA) OrElse Double.IsNaN(withinB) Then Return Double.NaN
        If withinA <= 0.0 OrElse withinB <= 0.0 Then Return Double.NaN

        Return crossMean / Math.Sqrt(withinA * withinB)
    End Function

    Private Function MeanAbsoluteCrossCorrelations(ByVal itemsA As List(Of String),
                                                   ByVal itemsB As List(Of String),
                                                   ByVal cor(,) As Double,
                                                   ByVal rowNames() As String,
                                                   ByVal colNames() As String) As Double
        Dim total As Double = 0.0
        Dim count As Integer = 0

        For Each itemA As String In itemsA
            For Each itemB As String In itemsB
                Dim r As Double = MatrixValue(cor, rowNames, colNames, itemA, itemB)
                If Double.IsNaN(r) Then Return Double.NaN
                total += Math.Abs(r)
                count += 1
            Next
        Next

        If count = 0 Then Return Double.NaN
        Return total / CDbl(count)
    End Function

    Private Function MeanAbsoluteWithinCorrelations(ByVal items As List(Of String),
                                                    ByVal cor(,) As Double,
                                                    ByVal rowNames() As String,
                                                    ByVal colNames() As String) As Double
        If items.Count < 2 Then Return Double.NaN

        Dim total As Double = 0.0
        Dim count As Integer = 0

        For i As Integer = 0 To items.Count - 2
            For j As Integer = i + 1 To items.Count - 1
                Dim r As Double = MatrixValue(cor, rowNames, colNames, items(i), items(j))
                If Double.IsNaN(r) Then Return Double.NaN
                total += Math.Abs(r)
                count += 1
            Next
        Next

        If count = 0 Then Return Double.NaN
        Return total / CDbl(count)
    End Function

    Private Function IndexOfName(ByVal names() As String, ByVal targetName As String) As Integer
        If names Is Nothing OrElse targetName Is Nothing Then Return -1

        For i As Integer = 0 To names.Length - 1
            If String.Equals(names(i), targetName, StringComparison.OrdinalIgnoreCase) Then
                Return i
            End If
        Next

        'A small fallback for AMOS builds that include leading/trailing spaces
        'in matrix row or column labels.
        Dim wanted As String = targetName.Trim()
        For i As Integer = 0 To names.Length - 1
            If names(i) IsNot Nothing AndAlso _
               String.Equals(names(i).Trim(), wanted, StringComparison.OrdinalIgnoreCase) Then
                Return i
            End If
        Next

        Return -1
    End Function

    Private Function MatrixValue(ByVal matrix(,) As Double,
                                 ByVal rowNames() As String,
                                 ByVal colNames() As String,
                                 ByVal rowName As String,
                                 ByVal colName As String) As Double
        If matrix Is Nothing OrElse rowNames Is Nothing OrElse colNames Is Nothing Then Return Double.NaN

        Dim r As Integer = IndexOfName(rowNames, rowName)
        Dim c As Integer = IndexOfName(colNames, colName)
        If r < 0 OrElse c < 0 Then Return Double.NaN

        Try
            Return matrix(r, c)
        Catch
            Return Double.NaN
        End Try
    End Function

    '============================
    ' AMOS output parsing
    '============================

    Private Function ReadAmosOutputText(ByVal outputPath As String,
                                            ByVal previousWriteTicks As Long,
                                            ByVal previousLength As Long) As String
        'Read the AMOS Graphics output only after the new analysis has finished
        'writing it. This avoids accidentally reading a previous/stale CFI.
        Dim lastText As String = ""
        Dim lastWriteTicks As Long = -1
        Dim lastLength As Long = -1
        Dim stableCount As Integer = 0

        For attempt As Integer = 0 To 100
            Try
                If File.Exists(outputPath) Then
                    Dim info As New FileInfo(outputPath)
                    Dim writeTicks As Long = info.LastWriteTimeUtc.Ticks
                    Dim fileLength As Long = info.Length

                    Dim looksFresh As Boolean = (previousWriteTicks = 0) OrElse _
                                                (writeTicks <> previousWriteTicks) OrElse _
                                                (fileLength <> previousLength)

                    If looksFresh AndAlso fileLength > 0 Then
                        Dim text As String = File.ReadAllText(outputPath)
                        If text IsNot Nothing AndAlso text.Length > 0 Then
                            lastText = text

                            If writeTicks = lastWriteTicks AndAlso fileLength = lastLength Then
                                stableCount += 1
                            Else
                                stableCount = 0
                            End If

                            lastWriteTicks = writeTicks
                            lastLength = fileLength

                            'Two consecutive stable reads are enough to regard the
                            'output as completely written.
                            If stableCount >= 2 Then Return lastText
                        End If
                    End If
                End If
            Catch
                'AMOS can briefly lock/replace the file. Retry below.
            End Try

            System.Threading.Thread.Sleep(100)
        Next

        'If the timestamp did not change (for example on an unusual file system),
        'fall back to the best readable copy rather than failing the whole report.
        If lastText <> "" Then Return lastText
        Try
            If File.Exists(outputPath) Then Return File.ReadAllText(outputPath)
        Catch
        End Try
        Return ""
    End Function

    Private Function ReadCfiFromAmosOutput(ByVal text As String,
                                           ByVal engineModelCmin As Double,
                                           ByVal engineModelDf As Integer) As Double
        Try
            If text Is Nothing OrElse text = "" Then Return Double.NaN

            'PRIMARY ROUTE: use the actual Baseline Comparisons table that contains
            'the CFI column and the Default model row. AMOS can repeat the caption in
            'the output file, so deliberately keep the LAST matching table. Within that
            'table, keep the LAST row containing "Default model" and then take the LAST
            'numeric value in that row. That is exactly the CFI cell displayed by AMOS.
            Dim baselineBody As String = FindLastBaselineComparisonsBody(text)
            Dim baselineRows As List(Of String) = ExtractRows(baselineBody)

            Dim defaultRow As String = ""
            For Each row As String In baselineRows
                Dim plainRow As String = DecodeBasicEntities(StripMarkup(row)).Trim()
                If plainRow.IndexOf("Default model", StringComparison.OrdinalIgnoreCase) >= 0 Then
                    defaultRow = row
                End If
            Next

            If defaultRow <> "" Then
                Dim directCfi As Double = LastNumericCellValue(defaultRow)
                'Do not clamp a directly read AMOS value; reproduce the printed CFI.
                If Not Double.IsNaN(directCfi) Then Return directCfi
            End If

            'If the fitted model has been renamed, use the last non-saturated,
            'non-independence model row and again take its final numeric cell.
            Dim fittedRow As String = ""
            For Each row As String In baselineRows
                Dim label As String = OutputCellText(row, 1)
                Dim lastValue As Double = LastNumericCellValue(row)
                If label <> "" AndAlso Not Double.IsNaN(lastValue) Then
                    If label.IndexOf("Saturated", StringComparison.OrdinalIgnoreCase) < 0 AndAlso _
                       label.IndexOf("Independence", StringComparison.OrdinalIgnoreCase) < 0 Then
                        fittedRow = row
                    End If
                End If
            Next

            If fittedRow <> "" Then
                Dim directCfi As Double = LastNumericCellValue(fittedRow)
                If Not Double.IsNaN(directCfi) Then Return Clamp01(directCfi)
            End If

            'SECOND ROUTE: calculate CFI from the CMIN values that AMOS itself
            'printed for the fitted model and the independence model. Using both
            'values from the same AMOS output prevents a mismatch between the
            'Graphics fit and a separate engine fit.
            Dim cminBody As String = FindTableBodyByNodeCaption(text, "CMIN")
            Dim modelCmin As Double = Double.NaN
            Dim modelDf As Double = Double.NaN
            Dim independenceCmin As Double = Double.NaN
            Dim independenceDf As Double = Double.NaN

            For Each row As String In ExtractRows(cminBody)
                Dim label As String = OutputCellText(row, 1)
                Dim rowCmin As Double = OutputCellNumber(row, 3)
                Dim rowDf As Double = OutputCellNumber(row, 4)

                If label.IndexOf("Independence", StringComparison.OrdinalIgnoreCase) >= 0 Then
                    If Not Double.IsNaN(rowCmin) AndAlso Not Double.IsNaN(rowDf) Then
                        independenceCmin = rowCmin
                        independenceDf = rowDf
                    End If
                ElseIf label.IndexOf("Saturated", StringComparison.OrdinalIgnoreCase) < 0 Then
                    If Double.IsNaN(modelCmin) AndAlso Not Double.IsNaN(rowCmin) AndAlso Not Double.IsNaN(rowDf) Then
                        modelCmin = rowCmin
                        modelDf = rowDf
                    End If
                End If
            Next

            If Double.IsNaN(modelCmin) Then modelCmin = engineModelCmin
            If Double.IsNaN(modelDf) Then modelDf = CDbl(engineModelDf)

            Dim calculated As Double = CalculateCfi(modelCmin, modelDf, independenceCmin, independenceDf)
            If Not Double.IsNaN(calculated) Then Return calculated

            'LAST ROUTE: search every row in the output for an Independence model
            'CMIN row. This protects against minor AMOS 31 output-layout changes.
            Dim independenceRow As String = FindNumericModelRowAnywhere(text, "Independence")
            independenceCmin = OutputCellNumber(independenceRow, 3)
            independenceDf = OutputCellNumber(independenceRow, 4)
            calculated = CalculateCfi(engineModelCmin, CDbl(engineModelDf), independenceCmin, independenceDf)
            If Not Double.IsNaN(calculated) Then Return calculated

            Return Double.NaN
        Catch
            Return Double.NaN
        End Try
    End Function

    Private Function CalculateCfi(ByVal modelCmin As Double,
                                  ByVal modelDf As Double,
                                  ByVal independenceCmin As Double,
                                  ByVal independenceDf As Double) As Double
        If Double.IsNaN(modelCmin) OrElse Double.IsNaN(modelDf) OrElse _
           Double.IsNaN(independenceCmin) OrElse Double.IsNaN(independenceDf) Then Return Double.NaN

        Dim modelNcp As Double = Math.Max(modelCmin - modelDf, 0.0)
        Dim independenceNcp As Double = Math.Max(independenceCmin - independenceDf, 0.0)
        Dim denominator As Double = Math.Max(Math.Max(modelNcp, independenceNcp), 0.0)
        If denominator <= 0.0 Then Return Double.NaN

        Return Clamp01(1.0 - (modelNcp / denominator))
    End Function

    Private Function Clamp01(ByVal value As Double) As Double
        If Double.IsNaN(value) Then Return Double.NaN
        If value < 0.0 Then Return 0.0
        If value > 1.0 Then Return 1.0
        Return value
    End Function

    Private Function FindNumericModelRowAnywhere(ByVal text As String, ByVal labelFragment As String) As String
        If text Is Nothing OrElse text = "" Then Return ""

        Dim p As Integer = 0
        While p < text.Length
            Dim rowStart As Integer = text.IndexOf("<tr", p, StringComparison.OrdinalIgnoreCase)
            If rowStart < 0 Then Exit While
            Dim rowEnd As Integer = text.IndexOf("</tr>", rowStart, StringComparison.OrdinalIgnoreCase)
            If rowEnd < 0 Then Exit While
            rowEnd += 5

            Dim row As String = text.Substring(rowStart, rowEnd - rowStart)
            Dim plain As String = DecodeBasicEntities(StripMarkup(row))
            If plain.IndexOf(labelFragment, StringComparison.OrdinalIgnoreCase) >= 0 Then
                Dim cmin As Double = OutputCellNumber(row, 3)
                Dim df As Double = OutputCellNumber(row, 4)
                If Not Double.IsNaN(cmin) AndAlso Not Double.IsNaN(df) Then Return row
            End If
            p = rowEnd
        End While

        Return ""
    End Function

    Private Function ReadLatentCorrelationsFromAmosOutput(ByVal text As String,
                                                           ByVal constructs As List(Of ConstructInfo)) As Dictionary(Of String, Double)
        Dim result As New Dictionary(Of String, Double)(StringComparer.OrdinalIgnoreCase)
        If text Is Nothing OrElse text = "" Then Return result

        Dim body As String = FindTableBodyByNodeCaption(text, "Correlations:")
        If body = "" Then Return result

        Dim rows As List(Of String) = ExtractRows(body)
        For Each row As String In rows
            'AMOS standardized Correlations table columns are:
            'variable 1 | <--> | variable 2 | estimate
            Dim leftName As String = OutputCellText(row, 1)
            Dim rightName As String = OutputCellText(row, 3)
            Dim estimate As Double = OutputCellNumber(row, 4)

            If leftName <> "" AndAlso rightName <> "" AndAlso Not Double.IsNaN(estimate) Then
                If HasConstruct(constructs, leftName) AndAlso HasConstruct(constructs, rightName) Then
                    result(PairKey(leftName, rightName)) = estimate
                End If
            End If
        Next

        Return result
    End Function

    Private Function PairKey(ByVal a As String, ByVal b As String) As String
        If a Is Nothing Then a = ""
        If b Is Nothing Then b = ""
        a = a.Trim()
        b = b.Trim()

        If String.Compare(a, b, StringComparison.OrdinalIgnoreCase) <= 0 Then
            Return a & ChrW(30) & b
        Else
            Return b & ChrW(30) & a
        End If
    End Function

    Private Function FindLastBaselineComparisonsBody(ByVal text As String) As String
        If text Is Nothing OrElse text = "" Then Return ""

        Dim searchPos As Integer = 0
        Dim bestBody As String = ""

        While searchPos < text.Length
            Dim p As Integer = text.IndexOf("Baseline Comparisons", searchPos, StringComparison.OrdinalIgnoreCase)
            If p < 0 Then Exit While

            Dim tbodyStart As Integer = text.IndexOf("<tbody", p, StringComparison.OrdinalIgnoreCase)
            If tbodyStart >= 0 Then
                Dim bodyStart As Integer = text.IndexOf(">", tbodyStart)
                If bodyStart >= 0 Then
                    bodyStart += 1
                    Dim bodyEnd As Integer = text.IndexOf("</tbody>", bodyStart, StringComparison.OrdinalIgnoreCase)
                    If bodyEnd >= 0 Then
                        Dim candidate As String = text.Substring(bodyStart, bodyEnd - bodyStart)
                        Dim plain As String = DecodeBasicEntities(StripMarkup(candidate))

                        'Require both the CFI header and the fitted model row so that a
                        'navigation caption or unrelated occurrence cannot be mistaken
                        'for the numeric Baseline Comparisons table.
                        If plain.IndexOf("CFI", StringComparison.OrdinalIgnoreCase) >= 0 AndAlso _
                           plain.IndexOf("Default model", StringComparison.OrdinalIgnoreCase) >= 0 Then
                            bestBody = candidate
                        End If
                    End If
                End If
            End If

            searchPos = p + "Baseline Comparisons".Length
        End While

        'Fallback retains compatibility if AMOS changes the header text slightly.
        If bestBody = "" Then bestBody = FindTableBodyByNodeCaption(text, "Baseline Comparisons")
        Return bestBody
    End Function

    Private Function FindTableBodyByNodeCaption(ByVal text As String, ByVal caption As String) As String
        If text Is Nothing OrElse caption Is Nothing Then Return ""

        Dim p As Integer = text.IndexOf("nodecaption=""" & caption & """", StringComparison.OrdinalIgnoreCase)
        If p < 0 Then p = text.IndexOf("nodecaption='" & caption & "'", StringComparison.OrdinalIgnoreCase)
        If p < 0 Then p = text.IndexOf(caption, StringComparison.OrdinalIgnoreCase)
        If p < 0 Then Return ""

        Dim tbodyStart As Integer = text.IndexOf("<tbody", p, StringComparison.OrdinalIgnoreCase)
        If tbodyStart < 0 Then Return ""

        Dim bodyStart As Integer = text.IndexOf(">", tbodyStart)
        If bodyStart < 0 Then Return ""
        bodyStart += 1

        Dim bodyEnd As Integer = text.IndexOf("</tbody>", bodyStart, StringComparison.OrdinalIgnoreCase)
        If bodyEnd < 0 Then Return ""

        Return text.Substring(bodyStart, bodyEnd - bodyStart)
    End Function

    Private Function ExtractRows(ByVal tableBody As String) As List(Of String)
        Dim rows As New List(Of String)()
        If tableBody Is Nothing OrElse tableBody = "" Then Return rows

        Dim p As Integer = 0
        While p < tableBody.Length
            Dim rowStart As Integer = tableBody.IndexOf("<tr", p, StringComparison.OrdinalIgnoreCase)
            If rowStart < 0 Then Exit While

            Dim rowEnd As Integer = tableBody.IndexOf("</tr>", rowStart, StringComparison.OrdinalIgnoreCase)
            If rowEnd < 0 Then Exit While
            rowEnd += 5

            rows.Add(tableBody.Substring(rowStart, rowEnd - rowStart))
            p = rowEnd
        End While

        Return rows
    End Function

    Private Function FindRowContaining(ByVal tableBody As String, ByVal target As String) As String
        If tableBody Is Nothing OrElse target Is Nothing Then Return ""

        For Each row As String In ExtractRows(tableBody)
            Dim plain As String = DecodeBasicEntities(StripMarkup(row))
            If plain.IndexOf(target, StringComparison.OrdinalIgnoreCase) >= 0 Then Return row
        Next

        Return ""
    End Function

    Private Function OutputCellText(ByVal row As String, ByVal cellNumber As Integer) As String
        Dim openingTag As String = ""
        Dim inner As String = GetOutputCell(row, cellNumber, openingTag)
        If inner = "" Then Return ""
        Return DecodeBasicEntities(StripMarkup(inner)).Trim()
    End Function

    Private Function OutputCellNumber(ByVal row As String, ByVal cellNumber As Integer) As Double
        Dim openingTag As String = ""
        Dim inner As String = GetOutputCell(row, cellNumber, openingTag)
        If openingTag = "" Then Return Double.NaN

        'Most AMOS numerical output cells store the full-precision value in an x="..."
        'attribute. Use it when available; otherwise fall back to the displayed text.
        Dim xValue As String = ExtractAttribute(openingTag, "x")
        Dim value As Double = ParseAmosNumber(xValue)
        If Not Double.IsNaN(value) Then Return value

        Return ParseAmosNumber(DecodeBasicEntities(StripMarkup(inner)).Trim())
    End Function

    Private Function GetOutputCell(ByVal row As String,
                                   ByVal cellNumber As Integer,
                                   ByRef openingTag As String) As String
        openingTag = ""
        If row Is Nothing OrElse cellNumber < 1 Then Return ""

        Dim searchPos As Integer = 0
        For i As Integer = 1 To cellNumber
            Dim tdStart As Integer = row.IndexOf("<td", searchPos, StringComparison.OrdinalIgnoreCase)
            If tdStart < 0 Then Return ""

            Dim tagEnd As Integer = row.IndexOf(">", tdStart)
            If tagEnd < 0 Then Return ""

            Dim tdEnd As Integer = row.IndexOf("</td>", tagEnd + 1, StringComparison.OrdinalIgnoreCase)
            If tdEnd < 0 Then Return ""

            If i = cellNumber Then
                openingTag = row.Substring(tdStart, tagEnd - tdStart + 1)
                Return row.Substring(tagEnd + 1, tdEnd - tagEnd - 1)
            End If

            searchPos = tdEnd + 5
        Next

        Return ""
    End Function

    Private Function LastNumericCellValue(ByVal row As String) As Double
        If row Is Nothing OrElse row = "" Then Return Double.NaN

        Dim searchPos As Integer = 0
        Dim lastValue As Double = Double.NaN

        While searchPos < row.Length
            Dim tdStart As Integer = row.IndexOf("<td", searchPos, StringComparison.OrdinalIgnoreCase)
            If tdStart < 0 Then Exit While

            Dim tagEnd As Integer = row.IndexOf(">", tdStart)
            If tagEnd < 0 Then Exit While
            Dim tdEnd As Integer = row.IndexOf("</td>", tagEnd + 1, StringComparison.OrdinalIgnoreCase)
            If tdEnd < 0 Then Exit While

            Dim openingTag As String = row.Substring(tdStart, tagEnd - tdStart + 1)
            Dim inner As String = row.Substring(tagEnd + 1, tdEnd - tagEnd - 1)

            Dim value As Double = ParseAmosNumber(ExtractAttribute(openingTag, "x"))
            If Double.IsNaN(value) Then
                value = ParseAmosNumber(DecodeBasicEntities(StripMarkup(inner)).Trim())
            End If
            If Not Double.IsNaN(value) Then lastValue = value

            searchPos = tdEnd + 5
        End While

        Return lastValue
    End Function

    Private Function ExtractAttribute(ByVal tagText As String, ByVal attributeName As String) As String
        If tagText Is Nothing OrElse attributeName Is Nothing Then Return ""

        Dim p As Integer = tagText.IndexOf(attributeName & "=", StringComparison.OrdinalIgnoreCase)
        If p < 0 Then Return ""
        p += attributeName.Length + 1
        If p >= tagText.Length Then Return ""

        Dim quote As Char = tagText.Chars(p)
        If quote = """"c OrElse quote = "'"c Then
            Dim qEnd As Integer = tagText.IndexOf(quote, p + 1)
            If qEnd < 0 Then Return ""
            Return tagText.Substring(p + 1, qEnd - p - 1)
        End If

        Dim valueEnd As Integer = p
        While valueEnd < tagText.Length AndAlso Not Char.IsWhiteSpace(tagText.Chars(valueEnd)) AndAlso tagText.Chars(valueEnd) <> ">"c
            valueEnd += 1
        End While
        Return tagText.Substring(p, valueEnd - p)
    End Function

    Private Function DecodeBasicEntities(ByVal text As String) As String
        If text Is Nothing Then Return ""
        Return text.Replace("&nbsp;", " ").Replace("&lt;", "<").Replace("&gt;", ">").Replace("&quot;", """").Replace("&#39;", "'").Replace("&amp;", "&")
    End Function

    Private Function StripMarkup(ByVal text As String) As String
        If text Is Nothing Then Return ""

        Dim sb As New StringBuilder()
        Dim insideTag As Boolean = False
        For Each ch As Char In text
            If ch = "<"c Then
                insideTag = True
            ElseIf ch = ">"c Then
                insideTag = False
            ElseIf Not insideTag Then
                sb.Append(ch)
            End If
        Next
        Return sb.ToString()
    End Function

    Private Function ParseAmosNumber(ByVal text As String) As Double
        If text Is Nothing Then Return Double.NaN
        Dim s As String = text.Trim()
        If s = "" Then Return Double.NaN

        Dim value As Double
        If Double.TryParse(s, NumberStyles.Any, CultureInfo.CurrentCulture, value) Then Return value
        If Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, value) Then Return value
        Return Double.NaN
    End Function

    '============================
    ' HTML report
    '============================

    Private Function BuildReportPath() As String
        Dim projectBase As String = Amos.Pd.ProjectName
        If projectBase IsNot Nothing AndAlso projectBase.Trim() <> "" Then
            Return projectBase & "_CFA_Report.html"
        End If

        Return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "AMOS_CFA_Report.html")
    End Function

    Private Sub WriteHtmlReport(ByVal reportPath As String,
                                ByVal constructs As List(Of ConstructInfo),
                                ByVal sem As AmosEngineLib.AmosEngine,
                                ByVal cfi As Double,
                                ByVal cov(,) As Double,
                                ByVal covRows() As String,
                                ByVal covCols() As String,
                                ByVal cor(,) As Double,
                                ByVal corRows() As String,
                                ByVal corCols() As String,
                                ByVal latentCorrelations As Dictionary(Of String, Double))

        Dim sb As New StringBuilder()
        sb.AppendLine("<!DOCTYPE html>")
        sb.AppendLine("<html><head><meta charset='utf-8'><title>AMOS CFA One-Click Report</title>")
        sb.AppendLine("<style>")
        sb.AppendLine("body{font-family:Segoe UI,Arial,sans-serif;margin:30px;color:#222;line-height:1.4}")
        sb.AppendLine("h1{margin-bottom:4px} h2{margin-top:34px;border-bottom:2px solid #ddd;padding-bottom:6px}")
        sb.AppendLine("table{border-collapse:collapse;margin:12px 0 20px 0;min-width:560px}")
        sb.AppendLine("th,td{border:1px solid #cfcfcf;padding:7px 10px;text-align:right;vertical-align:top}")
        sb.AppendLine("th{background:#f3f3f3;font-weight:600} th:first-child,td:first-child{text-align:left}")
        sb.AppendLine(".note{background:#f7f7f7;border-left:4px solid #aaa;padding:10px 12px;margin:12px 0}")
        sb.AppendLine(".small{font-size:0.90em;color:#555}.na{color:#888}.diag{font-weight:700}")
        sb.AppendLine("</style></head><body>")

        sb.AppendLine("<h1>Confirmatory Factor Analysis Report</h1>")
        sb.AppendLine("<div class='small'>Generated from the model currently drawn in IBM SPSS Amos.</div>")
        sb.AppendLine("<div class='note'><b>Scope:</b> Version 1.6 supports a single-group, first-order reflective CFA with each observed indicator assigned to one construct.</div>")

        'Table 1: model fit
        sb.AppendLine("<h2>Table 1. Goodness-of-fit indices</h2>")
        sb.AppendLine("<table><tr><th>Index</th><th>Value</th></tr>")
        HtmlMetricRow(sb, "Chi-square (CMIN)", sem.Cmin)
        HtmlMetricRow(sb, "Degrees of freedom", CDbl(sem.Df), "0")
        HtmlMetricRow(sb, "p-value", sem.P)
        If sem.Df <> 0 Then
            HtmlMetricRow(sb, "CMIN/df", sem.Cmin / CDbl(sem.Df))
        Else
            HtmlMetricRow(sb, "CMIN/df", Double.NaN)
        End If
        HtmlMetricRow(sb, "Comparative Fit Index (CFI)", cfi)
        HtmlMetricRow(sb, "RMSEA", sem.Rmsea)
        sb.AppendLine("<tr><td>RMSEA 90% CI</td><td>" & Fmt(sem.RmseaLo) & " &ndash; " & Fmt(sem.RmseaHi) & "</td></tr>")
        HtmlMetricRow(sb, "PCLOSE", sem.Pclose)
        sb.AppendLine("</table>")

        'Table 2: loadings + reliability/AVE
        sb.AppendLine("<h2>Table 2. Standardized factor loadings, AVE and reliability</h2>")
        sb.AppendLine("<table><tr><th>Indicator</th>")
        For Each c As ConstructInfo In constructs
            sb.Append("<th>" & H(c.DisplayName) & "</th>")
        Next
        sb.AppendLine("</tr>")

        For Each cRow As ConstructInfo In constructs
            For Each item As String In cRow.Items
                sb.Append("<tr><td>" & H(item) & "</td>")
                For Each cCol As ConstructInfo In constructs
                    If cCol.Name.Equals(cRow.Name, StringComparison.OrdinalIgnoreCase) Then
                        Dim loading As Double = Double.NaN
                        If cCol.Loadings.ContainsKey(item) Then loading = cCol.Loadings(item)
                        sb.Append("<td>" & Fmt(loading) & "</td>")
                    Else
                        sb.Append("<td>&mdash;</td>")
                    End If
                Next
                sb.AppendLine("</tr>")
            Next
        Next

        sb.Append("<tr><td><b>AVE</b></td>")
        For Each c As ConstructInfo In constructs
            sb.Append("<td><b>" & Fmt(c.Ave) & "</b></td>")
        Next
        sb.AppendLine("</tr>")

        sb.Append("<tr><td><b>Cronbach's alpha</b></td>")
        For Each c As ConstructInfo In constructs
            sb.Append("<td><b>" & Fmt(c.Alpha) & "</b></td>")
        Next
        sb.AppendLine("</tr></table>")
        sb.AppendLine("<div class='small'>AVE = mean of squared standardized loadings. Cronbach's alpha is computed from the indicators' sample covariance matrix.</div>")

        'Table 3: Fornell-Larcker
        sb.AppendLine("<h2>Table 3. Fornell-Larcker criterion</h2>")
        sb.AppendLine("<table><tr><th>Construct</th>")
        For Each c As ConstructInfo In constructs
            sb.Append("<th>" & H(c.DisplayName) & "</th>")
        Next
        sb.AppendLine("</tr>")

        For i As Integer = 0 To constructs.Count - 1
            sb.Append("<tr><td>" & H(constructs(i).DisplayName) & "</td>")
            For j As Integer = 0 To constructs.Count - 1
                If i = j Then
                    Dim rootAve As Double = If(Double.IsNaN(constructs(i).Ave) OrElse constructs(i).Ave < 0.0, Double.NaN, Math.Sqrt(constructs(i).Ave))
                    sb.Append("<td class='diag'>" & Fmt(rootAve) & "</td>")
                ElseIf j > i Then
                    'Conventional Fornell-Larcker presentation: show one triangle only.
                    sb.Append("<td></td>")
                Else
                    Dim r As Double = Double.NaN
                    Dim key As String = PairKey(constructs(i).Name, constructs(j).Name)
                    If latentCorrelations IsNot Nothing AndAlso latentCorrelations.ContainsKey(key) Then
                        r = latentCorrelations(key)
                    End If
                    sb.Append("<td>" & Fmt(r) & "</td>")
                End If
            Next
            sb.AppendLine("</tr>")
        Next
        sb.AppendLine("</table>")
        sb.AppendLine("<div class='small'>Diagonal (bold) = square root of AVE, not 1.000. The lower triangle contains the signed, unsquared standardized latent-factor correlations (Phi) reported by AMOS; the upper triangle is left blank. For discriminant-validity assessment, compare each square root of AVE with the absolute magnitude of the relevant Phi correlation. This is mathematically equivalent to comparing AVE with Phi squared.</div>")

        'Table 4: HTMT
        sb.AppendLine("<h2>Table 4. Heterotrait-monotrait ratio (HTMT)</h2>")
        sb.AppendLine("<table><tr><th>Construct</th>")
        For Each c As ConstructInfo In constructs
            sb.Append("<th>" & H(c.DisplayName) & "</th>")
        Next
        sb.AppendLine("</tr>")

        For i As Integer = 0 To constructs.Count - 1
            sb.Append("<tr><td>" & H(constructs(i).DisplayName) & "</td>")
            For j As Integer = 0 To constructs.Count - 1
                If i = j Then
                    sb.Append("<td>&mdash;</td>")
                ElseIf j > i Then
                    sb.Append("<td></td>")
                Else
                    Dim htmtValue As Double = Htmt(constructs(i), constructs(j), cor, corRows, corCols)
                    sb.Append("<td>" & Fmt(htmtValue) & "</td>")
                End If
            Next
            sb.AppendLine("</tr>")
        Next
        sb.AppendLine("</table>")
        sb.AppendLine("<div class='small'>HTMT uses the mean absolute correlations between indicators of different constructs divided by the geometric mean of the two within-construct mean absolute inter-item correlations.</div>")

        sb.AppendLine("<h2>Calculation notes</h2>")
        sb.AppendLine("<ul>")
        sb.AppendLine("<li>This report does not change the path diagram.</li>")
        sb.AppendLine("<li>CFI is read from the last matching Baseline Comparisons table: the last numeric value in the last Default model row (the CFI cell shown by AMOS). Only if that cell cannot be read does the plugin use a fallback.</li>")
        sb.AppendLine("<li>Fornell-Larcker uses square root of AVE on the diagonal and the unsquared standardized latent-factor Phi correlations off the diagonal. Phi keeps its sign in the table, but the criterion is evaluated by magnitude: sqrt(AVE) must exceed |Phi|. Equivalently, AVE must exceed Phi squared. A 1.000 diagonal belongs to an ordinary correlation matrix and is not used in the conventional Fornell-Larcker table.</li>")
        sb.AppendLine("<li>Report tables use PDElement.LongLabel for each individual latent variable when available. Duplicate labels are never repeated across all constructs; the plugin falls back to indicator stems or unique AMOS internal names.</li>")
        sb.AppendLine("<li>For data with missing values, Cronbach's alpha and HTMT use the sample moments made available by AMOS and can differ from separately configured SPSS procedures.</li>")
        sb.AppendLine("</ul>")

        sb.AppendLine("</body></html>")

        File.WriteAllText(reportPath, sb.ToString(), Encoding.UTF8)
    End Sub

    Private Sub HtmlMetricRow(ByVal sb As StringBuilder, ByVal label As String, ByVal value As Double, Optional ByVal format As String = "0.000")
        sb.AppendLine("<tr><td>" & H(label) & "</td><td>" & Fmt(value, format) & "</td></tr>")
    End Sub

    Private Function Fmt(ByVal value As Double, Optional ByVal format As String = "0.000") As String
        If Double.IsNaN(value) OrElse Double.IsInfinity(value) Then Return "<span class='na'>N/A</span>"
        Return value.ToString(format, CultureInfo.InvariantCulture)
    End Function

    Private Function H(ByVal text As String) As String
        If text Is Nothing Then Return ""
        Return text.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("""", "&quot;")
    End Function
End Class
