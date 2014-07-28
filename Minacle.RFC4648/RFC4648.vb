Public NotInheritable Class RFC4648

  Private Shared ReadOnly base64 As Char() = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  Private Shared ReadOnly base64url As Char() = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  Private Shared ReadOnly base32 As Char() = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
  Private Shared ReadOnly base32hex As Char() = "0123456789ABCDEFGHIJKLMNOPQRSTUV"
  Private Shared ReadOnly base16 As Char() = "0123456789ABCDEF"
  Private Shared ReadOnly pad As Char = "="c

  Private Sub New()
    Throw New NotSupportedException
  End Sub

  Public Shared Function ToBase64(d As Byte()) As String
    Return ToBase64(d, base64)
  End Function

  Public Shared Function ToBase64Url(d As Byte()) As String
    Return ToBase64(d, base64url)
  End Function

  Private Shared Function ToBase64(d As Byte(), t As Char()) As String
    Dim b(4 * ((d.Length - 1) \ 3 + 1) - 1) As SByte
    For i = 0 To b.Length - 1
      b(i) = -1
    Next
    For i = 0 To d.Length - 1 Step 3
      Dim c = (i \ 3) * 4
      b(c) = (d(i) And 252) >> 2
      b(c + 1) = (d(i) And 3) << 4
      If i + 1 < d.Length Then
        b(c + 1) = b(c + 1) Or (d(i + 1) And 240) >> 4
        b(c + 2) = (d(i + 1) And 15) << 2
        If i + 2 < d.Length Then
          b(c + 2) = b(c + 2) Or (d(i + 2) And 192) >> 6
          b(c + 3) = (d(i + 2) And 63)
        End If
      End If
    Next
    Dim r As New StringBuilder
    For Each i In b
      If i < 0 Then
        r.Append(pad)
      Else
        r.Append(t(i))
      End If
    Next
    Return r.ToString
  End Function

  Public Shared Function FromBase64(s As String) As Byte()
    Return FromBase64(s, base64)
  End Function

  Public Shared Function FromBase64Url(s As String) As Byte()
    Return FromBase64(s, base64url)
  End Function

  Private Shared Function FromBase64(s As String, t As Char()) As Byte()
    Dim b(If(s.IndexOf(pad) < 0, s.Length, s.IndexOf(pad)) - 1) As SByte
    For i = 0 To b.Length - 1
      b(i) = Array.IndexOf(t, s(i))
    Next
    Dim d(Math.Ceiling(b.Length * 3 / 4) - 1) As Byte
    For i = 0 To b.Length - 1 Step 4
      Dim c = (i \ 4) * 3
      d(c) = b(i) << 2
      If i + 1 < b.Length Then
        d(c) = d(c) Or ((b(i + 1) And 48) >> 4)
        d(c + 1) = (b(i + 1) And 15) << 4
        If i + 2 < b.Length Then
          d(c + 1) = d(c + 1) Or ((b(i + 2) And 60) >> 2)
          d(c + 2) = (b(i + 2) And 3) << 6
          If i + 3 < b.Length Then
            d(c + 2) = d(c + 2) Or b(i + 3)
          End If
        End If
      End If
    Next
    Return d
  End Function

  Public Shared Function ToBase32(d As Byte()) As String
    Return ToBase32(d, base32)
  End Function

  Public Shared Function ToBase32Hex(d As Byte()) As String
    Return ToBase32(d, base32hex)
  End Function

  Private Shared Function ToBase32(d As Byte(), t As Char()) As String
    Dim b(8 * ((d.Length - 1) \ 5 + 1) - 1) As SByte
    For i = 0 To b.Length - 1
      b(i) = -1
    Next
    For i = 0 To d.Length - 1 Step 5
      Dim c = (i \ 5) * 8
      b(c) = (d(i) And 248) >> 3
      b(c + 1) = (d(i) And 7) << 2
      If i + 1 < d.Length Then
        b(c + 1) = b(c + 1) Or (d(i + 1) And 192) >> 6
        b(c + 2) = (d(i + 1) And 62) >> 1
        b(c + 3) = (d(i + 1) And 1) << 4
        If i + 2 < d.Length Then
          b(c + 3) = b(c + 3) Or (d(i + 2) And 240) >> 4
          b(c + 4) = (d(i + 2) And 15) << 1
          If i + 3 < d.Length Then
            b(c + 4) = b(c + 4) Or (d(i + 3) And 128) >> 7
            b(c + 5) = (d(i + 3) And 124) >> 2
            b(c + 6) = (d(i + 3) And 3) << 3
            If i + 4 < d.Length Then
              b(c + 6) = b(c + 6) Or (d(i + 4) And 224) >> 5
              b(c + 7) = (d(i + 4) And 31)
            End If
          End If
        End If
      End If
    Next
    Dim r As New StringBuilder
    For Each i In b
      If i < 0 Then
        r.Append(pad)
      Else
        r.Append(t(i))
      End If
    Next
    Return r.ToString
  End Function

  Public Shared Function FromBase32(s As String) As Byte()
    Return FromBase32(s, base32)
  End Function

  Public Shared Function FromBase32Hex(s As String) As Byte()
    Return FromBase32(s, base32hex)
  End Function

  Private Shared Function FromBase32(s As String, t As Char()) As Byte()
    Dim b(If(s.IndexOf(pad) < 0, s.Length, s.IndexOf(pad)) - 1) As SByte
    For i = 0 To b.Length - 1
      b(i) = Array.IndexOf(t, s(i))
    Next
    Dim d(Math.Ceiling(b.Length * 5 / 8) - 1) As Byte
    For i = 0 To b.Length - 1 Step 8
      Dim c = (i \ 8) * 5
      d(c) = b(i) << 3
      If i + 1 < b.Length Then
        d(c) = d(c) Or ((b(i + 1) And 28) >> 2)
        d(c + 1) = (b(i + 1) And 3) << 6
        If i + 2 < b.Length Then
          d(c + 1) = d(c + 1) Or (b(i + 2) << 1)
          If i + 3 < b.Length Then
            d(c + 1) = d(c + 1) Or ((b(i + 3) And 16) >> 4)
            d(c + 2) = (b(i + 3) And 15) << 4
            If i + 4 < b.Length Then
              d(c + 2) = d(c + 2) Or ((b(i + 4) And 30) >> 1)
              d(c + 3) = (b(i + 4) And 1) << 7
              If i + 5 < b.Length Then
                d(c + 3) = d(c + 3) Or (b(i + 5) << 2)
                If i + 6 < b.Length Then
                  d(c + 3) = d(c + 3) Or ((b(i + 6) And 24) >> 3)
                  d(c + 4) = (b(i + 6) And 7) << 5
                  If i + 7 < b.Length Then
                    d(c + 4) = d(c + 4) Or b(i + 7)
                  End If
                End If
              End If
            End If
          End If
        End If
      End If
    Next
    Return d
  End Function

  Public Shared Function ToBase16(d As Byte()) As String
    Dim b(d.Length * 2 - 1) As SByte
    For i = 0 To d.Length - 1
      b(i * 2) = (d(i) And 240) >> 4
      b(i * 2 + 1) = d(i) And 15
    Next
    Dim r As New StringBuilder
    For Each i In b
      r.Append(base16(i))
    Next
    Return r.ToString
  End Function

  Public Shared Function FromBase16(s As String) As Byte()
    Dim b(s.Length - 1) As SByte
    For i = 0 To b.Length - 1
      b(i) = Array.IndexOf(base16, s(i))
    Next
    Dim d(b.Length / 2 - 1) As Byte
    For i = 0 To b.Length - 1 Step 2
      d(i / 2) = (b(i) << 4) Or b(i + 1)
    Next
    Return d
  End Function
End Class
