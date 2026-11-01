# Drives a window that `windows_gui_events` opens: finds it by its title, checks that a pixel in
# the middle of it has the color the program shows, then posts it input, resizes it, and asks it
# to close, a step at a time so the program sees each in order. It prints `ok`, or what failed.
param([string]$Title, [int]$Color)

Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;
public static class Win {
    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern IntPtr FindWindowW(string className, string title);
    [DllImport("user32.dll")]
    public static extern bool PostMessageW(IntPtr window, uint message, UIntPtr w, IntPtr l);
    [DllImport("user32.dll")]
    public static extern bool GetClientRect(IntPtr window, out Rect rect);
    [DllImport("user32.dll")]
    public static extern bool GetWindowRect(IntPtr window, out Rect rect);
    [DllImport("user32.dll")]
    public static extern bool SetWindowPos(IntPtr window, IntPtr after, int x, int y, int width,
        int height, uint flags);
    [DllImport("user32.dll")]
    public static extern bool SetProcessDpiAwarenessContext(IntPtr context);
    [DllImport("user32.dll")]
    public static extern bool PrintWindow(IntPtr window, IntPtr dc, uint flags);
    [DllImport("user32.dll")]
    public static extern IntPtr GetDC(IntPtr window);
    [DllImport("user32.dll")]
    public static extern int ReleaseDC(IntPtr window, IntPtr dc);
    [DllImport("gdi32.dll")]
    public static extern IntPtr CreateCompatibleDC(IntPtr dc);
    [DllImport("gdi32.dll")]
    public static extern IntPtr CreateCompatibleBitmap(IntPtr dc, int width, int height);
    [DllImport("gdi32.dll")]
    public static extern IntPtr SelectObject(IntPtr dc, IntPtr bitmap);
    [DllImport("gdi32.dll")]
    public static extern uint GetPixel(IntPtr dc, int x, int y);
    [DllImport("gdi32.dll")]
    public static extern bool DeleteDC(IntPtr dc);
    [DllImport("gdi32.dll")]
    public static extern bool DeleteObject(IntPtr bitmap);
    [StructLayout(LayoutKind.Sequential)]
    public struct Rect { public int Left, Top, Right, Bottom; }
}
"@

[Win]::SetProcessDpiAwarenessContext([IntPtr](-4)) | Out-Null
$window = [IntPtr]::Zero
for ($i = 0; $i -lt 300 -and $window -eq [IntPtr]::Zero; $i++) {
    Start-Sleep -Milliseconds 100
    $window = [Win]::FindWindowW("stone", $Title)
}
if ($window -eq [IntPtr]::Zero) { Write-Output "no window titled '$Title'"; exit 1 }
# the program shows its canvas before it says so on its output, so wait for it to settle
Start-Sleep -Milliseconds 500

# the middle pixel, through the window's own content, whatever covers it on the screen
$client = New-Object Win+Rect
[Win]::GetClientRect($window, [ref]$client) | Out-Null
$screen = [Win]::GetDC([IntPtr]::Zero)
$dc = [Win]::CreateCompatibleDC($screen)
$bitmap = [Win]::CreateCompatibleBitmap($screen, $client.Right, $client.Bottom)
[Win]::SelectObject($dc, $bitmap) | Out-Null
# PW_CLIENTONLY and PW_RENDERFULLCONTENT
[Win]::PrintWindow($window, $dc, 3) | Out-Null
$pixel = [Win]::GetPixel($dc, [int]($client.Right / 2), [int]($client.Bottom / 2))
[Win]::DeleteObject($bitmap) | Out-Null
[Win]::DeleteDC($dc) | Out-Null
[Win]::ReleaseDC([IntPtr]::Zero, $screen) | Out-Null
# a COLORREF is 0x00BBGGRR
$rgb = (($pixel -band 0xff) -shl 16) -bor ($pixel -band 0xff00) -bor (($pixel -shr 16) -band 0xff)
if ($rgb -ne $Color) { Write-Output ("pixel is {0:x6}, not {1:x6}" -f $rgb, $Color) }

function Post([uint32]$message, [uint64]$w, [int64]$l) {
    [Win]::PostMessageW($window, $message, [UIntPtr]$w, [IntPtr]$l) | Out-Null
}
# F5 down, held so it repeats, and up; then Left
Post 0x100 0x74 0x003f0001
Post 0x100 0x74 0x403f0001
Post 0x101 0x74 0xc03f0001
Post 0x100 0x25 0x014b0001
Post 0x101 0x25 0xc14b0001
# text: e acute, a backspace that types nothing, and a character in two halves
Post 0x102 0xe9 1
Post 0x102 0x08 1
Post 0x102 0xd83d 1
Post 0x102 0xde00 1
# the mouse moves, the left button goes down and up, and it moves left of the window
Post 0x200 0 ((20 -shl 16) -bor 10)
Post 0x201 1 ((21 -shl 16) -bor 11)
Post 0x202 0 ((21 -shl 16) -bor 11)
Post 0x200 0 ((7 -shl 16) -bor 0xfffb)
# the wheel a step away, then two half steps back
Post 0x20a (120 -shl 16) 0
# (-60 in wParam's high word, which PowerShell's hexadecimal literals would make negative)
Post 0x20a 4291035136 0
Post 0x20a 4291035136 0
Start-Sleep -Milliseconds 500

# the inside becomes 300 by 150
$outer = New-Object Win+Rect
[Win]::GetWindowRect($window, [ref]$outer) | Out-Null
$width = 300 + ($outer.Right - $outer.Left) - $client.Right
$height = 150 + ($outer.Bottom - $outer.Top) - $client.Bottom
# SWP_NOMOVE and SWP_NOZORDER
[Win]::SetWindowPos($window, [IntPtr]::Zero, 0, 0, $width, $height, 6) | Out-Null
Start-Sleep -Milliseconds 500

Post 0x10 0 0
Write-Output "ok"
