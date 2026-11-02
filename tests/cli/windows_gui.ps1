# Drives a window that `windows_gui.rs` opens, found by its title.
#
# In `events` mode it checks that the window's inside is `Width` by `Height` logical pixels at
# its scale and that a pixel in the middle has the color the program shows, then posts it input,
# resizes it, and asks it to close, a step at a time so the program sees each in order. Points
# are posted in the screen's pixels, the smallest that fall in the logical pixel meant, so the
# program sees the same points at any scale.
#
# In `move` mode it starts Windows' own loop of moving the window with the keyboard, which holds
# up the window's thread until it ends, checks that the program goes on drawing meanwhile, ends
# the loop, and asks the window to close.
#
# It parks the real pointer in the screen's corner first, so a resize puts no window under it.
#
# It prints the window's scale, then `ok`, or what failed.
param([string]$Title, [int]$Color, [double]$Scale = 0, [int]$Width = 0, [int]$Height = 0,
    [string]$Mode = "events")

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
    public static extern bool SetCursorPos(int x, int y);
    [DllImport("user32.dll")]
    public static extern int GetSystemMetrics(int index);
    [DllImport("user32.dll")]
    public static extern bool SetProcessDpiAwarenessContext(IntPtr context);
    [DllImport("user32.dll")]
    public static extern uint GetDpiForWindow(IntPtr window);
    [DllImport("user32.dll")]
    public static extern uint GetWindowThreadProcessId(IntPtr window, IntPtr process);
    [DllImport("user32.dll")]
    public static extern bool GetGUIThreadInfo(uint thread, ref ThreadInfo info);
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
    [StructLayout(LayoutKind.Sequential)]
    public struct ThreadInfo {
        public int Size, Flags;
        public IntPtr Active, Focus, Capture, MenuOwner, MoveSize, Caret;
        public Rect CaretRect;
    }
}
"@

[Win]::SetProcessDpiAwarenessContext([IntPtr](-4)) | Out-Null
# park the real pointer in the screen's corner, since a window that grows under it would make
# Windows send a mouse move that the posted ones do not account for
[Win]::SetCursorPos([Win]::GetSystemMetrics(0) - 1, [Win]::GetSystemMetrics(1) - 1) | Out-Null
$window = [IntPtr]::Zero
for ($i = 0; $i -lt 300 -and $window -eq [IntPtr]::Zero; $i++) {
    Start-Sleep -Milliseconds 100
    $window = [Win]::FindWindowW("stone", $Title)
}
if ($window -eq [IntPtr]::Zero) { Write-Output "no window titled '$Title'"; exit 1 }
if ($Scale -eq 0) { $Scale = [Win]::GetDpiForWindow($window) / 96 }
Write-Output "scale $Scale"
# the program shows its canvas before it says so on its output, so wait for it to settle
Start-Sleep -Milliseconds 500

# the color of the middle pixel, through the window's own content, whatever covers it on the
# screen, as 0xRRGGBB
function Middle {
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
    (($pixel -band 0xff) -shl 16) -bor ($pixel -band 0xff00) -bor (($pixel -shr 16) -band 0xff)
}

function Post([uint32]$message, [uint64]$w, [int64]$l) {
    [Win]::PostMessageW($window, $message, [UIntPtr]$w, [IntPtr]$l) | Out-Null
}

# the screen's pixels `logical` ones take, as stone rounds them
function Physical([int]$logical) { [int][math]::Floor($logical * $Scale + 0.5) }

# the first of the screen's pixels in the logical pixel `logical`
function Inside([int]$logical) { [int][math]::Ceiling($logical * $Scale) }

# an lParam of the point `x`, `y` in logical pixels
function Point([int]$x, [int]$y) {
    ((Inside $y) -band 0xffff) -shl 16 -bor ((Inside $x) -band 0xffff)
}

if ($Mode -eq "move") {
    $first = Middle
    # SC_MOVE: Windows moves the window with the arrow keys until Enter or Escape
    Post 0x112 0xf010 0
    Start-Sleep -Milliseconds 700
    $thread = [Win]::GetWindowThreadProcessId($window, [IntPtr]::Zero)
    $info = New-Object Win+ThreadInfo
    $info.Size = [System.Runtime.InteropServices.Marshal]::SizeOf($info)
    [Win]::GetGUIThreadInfo($thread, [ref]$info) | Out-Null
    # GUI_INMOVESIZE
    if (($info.Flags -band 2) -eq 0) { Write-Output "the window is not being moved" }
    $before = Middle
    Start-Sleep -Milliseconds 500
    $after = Middle
    if ($before -eq $after) {
        Write-Output ("the window stayed {0:x6} while it was moved" -f $before)
    }
    # Escape ends the move
    Post 0x100 0x1b 0x00010001
    Post 0x101 0x1b 0xc0010001
    Start-Sleep -Milliseconds 300
    Post 0x10 0 0
    Write-Output "ok"
    exit 0
}

$client = New-Object Win+Rect
[Win]::GetClientRect($window, [ref]$client) | Out-Null
if ($client.Right -ne (Physical $Width) -or $client.Bottom -ne (Physical $Height)) {
    Write-Output ("the inside is {0}x{1}" -f $client.Right, $client.Bottom)
}
$rgb = Middle
if ($rgb -ne $Color) { Write-Output ("pixel is {0:x6}, not {1:x6}" -f $rgb, $Color) }

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
Post 0x200 0 (Point 10 20)
Post 0x201 1 (Point 11 21)
Post 0x202 0 (Point 11 21)
Post 0x200 0 (Point -5 7)
# the wheel a step away, then two half steps back
Post 0x20a (120 -shl 16) 0
# (-60 in wParam's high word, which PowerShell's hexadecimal literals would make negative)
Post 0x20a 4291035136 0
Post 0x20a 4291035136 0
Start-Sleep -Milliseconds 500

# the inside becomes 300 by 150 logical pixels
$outer = New-Object Win+Rect
[Win]::GetWindowRect($window, [ref]$outer) | Out-Null
$width = (Physical 300) + ($outer.Right - $outer.Left) - $client.Right
$height = (Physical 150) + ($outer.Bottom - $outer.Top) - $client.Bottom
# SWP_NOMOVE and SWP_NOZORDER
[Win]::SetWindowPos($window, [IntPtr]::Zero, 0, 0, $width, $height, 6) | Out-Null
Start-Sleep -Milliseconds 500

Post 0x10 0 0
Write-Output "ok"
