; ../bin/wsl/create-reload-ahk.sh should be helpful to reload this

; recommended defaults - google to know more in details
#NoEnv
#UseHook
#InstallKeybdHook
#SingleInstance force ; makes the new script to replace the old one when you click ahk file to open

SendMode Input
; end of defaults

; Helper escapes in case developing this script
;^!+Esc::ExitApp  ; Exit script with Ctrl+Alt+Shift+Escape key
;^!+p::Pause      ; Pause script with Ctrl+Alt+Shift+P
;^!+s::Suspend    ; Suspend script with Ctrl+Alt+Shift+S
;^!+r::Reload     ; Reload script with Ctrl+Alt+Shift+R

; Disable CapsLock
SetCapsLockState, AlwaysOff

; Single press Esc, Keep pressing Ctrl
CapsLock::
  Send {Ctrl DownTemp}
  KeyWait, CapsLock
  Send {Ctrl Up}
  if (A_PriorKey = "CapsLock") {
    Send {Esc}
  }
  return

; Switch language - https://medium.com/@jinhyoung/windows-%ED%95%9C%EC%98%81%EC%A0%84%ED%99%98%EC%9D%84-%EB%A7%A5-%EC%B2%98%EB%9F%BC-capslock%EC%9C%BC%EB%A1%9C-%ED%95%98%EB%8A%94-%EB%B0%A9%EB%B2%95-6137fa8c22c0
LCtrl::Send, {vk15sc1F2}

; mimicking https://ke-complex-modifications.pqrs.org/#spacefn
Space & h:: Left
Space & j:: Down
Space & k:: Up
Space & l:: Right
Space::Space ; without this Space will not work

; since Alt is in position of Apple Cmd key I want these to be:
!a::Send, ^a ; select all
!w::Send, ^w ; close window
!r::Send, ^r ; refresh
!c::Send, ^c ; copy
!x::Send, ^x ; cut
!v::Send, ^v ; paste
!s::Send, ^s ; save
!t::Send, ^t ; new tab
!f::Send, ^f ; find
!l::Send, ^l ; focus address bar
!z::Send, ^z ; undo
!+z::Send, ^+z ; redo
; thanks to https://www.reddit.com/r/AutoHotkey/comments/rsozy8/comment/hqo0hqp
!LButton::Send ^{LButton} ; mimic CMD+click - open in a new tab

; move wordly
<#Left::Send, ^{Left}
<#Right::Send, ^{Right}
; move to ends
<!Left::Send, {Home}
<!Right::Send, {End}

; mimic opening alfred
!Space:: Send ^{Esc}

; mimic Cmd Backspace
!Backspace::Send ^{Backspace}
