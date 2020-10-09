#include "inkey.ch"
#include "box.ch"

#define ROWS 6
#define COLS 7

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

    //There are 42 elements in this array
    LOCAL acBoxes := {;
                      '┌', '─', '┐', '│', '┘', '─';
                      , '└', '│', '╔', '═', '╗', '║';
                      , '╝', '═', '╚', '║', '╓', '╖';
                      , '╜', '╙', '╒', '╕', '╛', '╘';
                      , '░', '▒', '▓', '┤', '╣', '┴';
                      , '┬', '├', '┼', '╩', '╦', '╠';
                      , '╬', '█', '▄', '¦', '▀', '■';
                     } 
    LOCAL nLength := Len(acBoxes)
    LOCAL nMinRow := Window():center_row() - Int(nLength / ROWS)
    LOCAL nMinCol := Window():center_col() - Int(nLength / COLS)
    LOCAL nRow := nMinRow + 1
    LOCAL nCol := nMinCol + 2
    LOCAL nMaxRow := nMinRow + 2 * ROWS - 1
    LOCAL nMaxCol := nMinCol + 2 * COLS 
    LOCAL nIndexRow := 1
    LOCAL nIndexCol := 1
    LOCAL lSave := .F.
    LOCAL cOldScreen
    LOCAL cScreen
    LOCAL nKey

    SAVE SCREEN TO cOldScreen

    draw_boxes(acBoxes, nMinRow, nMinCol)

    SAVE SCREEN TO cScreen

    nMinRow += 1
    nMinCol += 2
    display_box(nRow, nCol)

    DO WHILE .T.

        nKey = Inkey(0)

        RESTORE SCREEN FROM cScreen

        DO CASE
            CASE nKey == K_ESC
                EXIT
            CASE nKey == K_DOWN
                IF nRow < nMaxRow
                    nRow += 2
                    ++nIndexRow
                ELSE
                    nRow := nMinRow
                    nIndexRow := 1
                ENDIF
            CASE nKey == K_UP
                IF nRow > nMinRow
                    nRow -= 2
                    --nIndexRow
                ELSE
                    nRow := nMaxRow
                    nIndexRow := ROWS
                ENDIF
            CASE nKey == K_LEFT
                IF nCol > nMinCol
                    nCol -= 2
                    --nIndexCol
                ELSE
                    nCol := nMaxCol
                    nIndexCol := COLS
                ENDIF
            CASE nKey == K_RIGHT
                IF nCol < nMaxCol
                    nCol += 2
                    ++nIndexCol
                ELSE
                    nCol := nMinCol
                    nIndexCol := 1
                ENDIF
            CASE nKey == K_CTRL_DOWN .OR. nKey == K_PGDN
                nIndexRow := ROWS
                nRow := nMaxRow
            CASE nKey == K_CTRL_UP .OR. nKey == K_PGUP
                nIndexRow := 1
                nRow := nMinRow
            CASE nKey == K_CTRL_LEFT
                nIndexCol := 1
                nCol := nMinCol
            CASE nKey == K_CTRL_RIGHT 
                nIndexCol := COLS
                nCol := nMaxCol
            CASE nKey == K_ENTER
                lSave := .T.
                EXIT
        ENDCASE

        display_box(nRow, nCol)
    ENDDO

    RESTORE SCREEN FROM cOldScreen

RETURN IF(lSave, acBoxes[COLS * (nIndexRow - 1) + nIndexCol], '')

STATIC PROCEDURE draw_boxes(acBoxes, nRow, nCol)

    LOCAL i, j

    DispBegin()
    FOR i := 1 TO ROWS
        FOR j := 1 TO COLS
            @ 2 * i + nRow, 2 * j + nCol SAY acBoxes[COLS * (i - 1) + j]
        NEXT
    NEXT
    DispEnd()
    
RETURN

STATIC PROCEDURE display_box(nRow, nCol)

    @ nRow, nCol - 1, nRow + 2, nCol + 1 BOX B_SINGLE

RETURN
