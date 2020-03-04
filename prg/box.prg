#include "inkey.ch"
#include "box.ch"

PROCEDURE select_box()

    LOCAL cOldHeader := Window():header(Config():get_config('SelectBoxHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('SelectBoxFooter'))
    LOCAL nOldWindow := WSelect()
    LOCAL oCurrentGet := GetActive()
    LOCAL nLength := Len(oCurrentGet:buffer)
    LOCAL cOldScreen
    LOCAL cBox
    
    WSelect(0)
    SAVE SCREEN TO cOldScreen

    DispBegin() 
    Window():clear_screen()
    Window():refresh_header_footer()
    DispEnd()

    cBox := get_box()
    IF !Empty(cBox)
        oCurrentGet:varPut(PadR(RTrim(oCurrentGet:buffer) + cBox, nLength))
    ENDIF

    RESTORE SCREEN FROM cOldScreen
    WSelect(nOldWindow)
    Window():header(cOldHeader)
    Window():footer(cOldFooter)

RETURN

STATIC FUNCTION get_box()

    LOCAL acBoxes := {'┌', '─', '┐', '│', '┘', '─', '└', '│', '╔', '═', '╗', '║', '╝', '═', '╚', '║', '╓', '╖', '╜', '╙', '╒', '╕', '╛', '╘'}
    LOCAL nLength := Len(acBoxes)
    LOCAL nRow := 2 * (Window():get_top() + 1)
    LOCAL nCol := 2 
    LOCAL nIndexRow := 1
    LOCAL nIndexCol := 1
    LOCAL lSave := .F.
    LOCAL cOldScreen
    LOCAL cScreen
    LOCAL nKey

    SAVE SCREEN TO cOldScreen

    draw_boxes(acBoxes, nRow)

    SAVE SCREEN TO cScreen

    ++nRow
    display_box(nRow, nCol)

    DO WHILE .T.

        nKey = Inkey(0)

        RESTORE SCREEN FROM cScreen

        DO CASE
            CASE nKey == K_ESC
                EXIT
            CASE nKey == K_DOWN
                IF nRow < 2 * (Window():get_top() + 1) - 1 + Int(nLength / 6) * 2
                    nRow += 2
                    ++nIndexRow
                ELSE
                    nRow := 2 * (Window():get_top() + 1) + 1
                    nIndexRow := 1
                ENDIF
            CASE nKey == K_UP
                IF nRow > 2 * (Window():get_top + 1) + 1
                    nRow -= 2
                    --nIndexRow
                ELSE
                    nRow := 2 * (Window():get_top() + 1) - 1 + Int(nLength / 6) * 2
                    nIndexRow := Int(nLength / 6)
                ENDIF
            CASE nKey == K_LEFT
                IF nCol > 2
                    nCol -= 2
                    --nIndexCol
                ELSE
                    nCol := Int(nLength / 4) * 2
                    nIndexCol := Int(nLength / 4)
                ENDIF
            CASE nKey == K_RIGHT
                IF nCol < (nLength / 4) * 2
                    nCol += 2
                    ++nIndexCol
                ELSE
                    nCol := 2
                    nIndexCol := 1
                ENDIF
            CASE nKey == K_CTRL_DOWN .OR. nKey == K_PGDN
                nIndexRow := Int(nLength / 6)
                nRow := 2 * (Window():get_top() + 1) - 1 + (nLength / 6) * 2
            CASE nKey == K_CTRL_UP .OR. nKey == K_PGUP
                nIndexRow := 1
                nRow := 2 * (Window():get_top() + 1) + 1
            CASE nKey == K_CTRL_LEFT
                nIndexCol := 1
                nCol := 2
            CASE nKey == K_CTRL_RIGHT 
                nIndexCol := Int(nLength / 4)
                nCol := (nLength / 4) * 2
            CASE nKey == K_ENTER
                lSave := .T.
                EXIT
        ENDCASE

        display_box(nRow, nCol)
    ENDDO

    RESTORE SCREEN FROM cOldScreen

RETURN IF(lSave, acBoxes[6 * (nIndexRow - 1) + nIndexCol], '')

STATIC PROCEDURE draw_boxes(acBoxes, nRow)

    LOCAL i, j

    DispBegin()
    FOR i := 1 TO 4
        FOR j := 1 TO 6
            @ 2 * i + nRow, 2 * j SAY acBoxes[6 * (i - 1) + j]
        NEXT
    NEXT
    DispEnd()
    
RETURN

STATIC PROCEDURE display_box(nRow, nCol)

    @ nRow, nCol - 1, nRow + 2, nCol + 1 BOX B_SINGLE

RETURN
