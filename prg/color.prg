#include "inkey.ch"
#include "box.ch"

PROCEDURE select_color()

    LOCAL cOldHeader := Window():header(Config():get_config('SelectColorHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('SelectColorFooter'))
    LOCAL nOldWindow := WSelect()
    LOCAL oCurrentGet := GetActive()
    LOCAL nLength := Len(oCurrentGet:buffer)
    LOCAL cOldScreen
    LOCAL cColor
    
    WSelect(0)
    SAVE SCREEN TO cOldScreen

    DispBegin() 
    Window():clear_screen()
    Window():refresh_header_footer()
    DispEnd()

    cColor := get_color()
    IF !Empty(cColor)
        oCurrentGet:varPut(PadR(RTrim(oCurrentGet:buffer) + ',' + cColor, nLength))
    ENDIF

    RESTORE SCREEN FROM cOldScreen
    WSelect(nOldWindow)
    Window():header(cOldHeader)
    Window():footer(cOldFooter)

RETURN

STATIC FUNCTION get_color()

    LOCAL acColors := {'N', 'B', 'G', 'BG', 'R', 'RB', 'GR', 'W'}
    LOCAL nLength := Len(acColors)
    LOCAL lPlus := .F.
    LOCAL nWidth := 5
    LOCAL nRow := 2 * (Window():get_top() + 1)
    LOCAL nCol := 2 * nWidth
    LOCAL nIndexRow := 1
    LOCAL nIndexCol := 1
    LOCAL lSave := .F.
    LOCAL cOldScreen
    LOCAL cScreen
    LOCAL nKey

    SAVE SCREEN TO cOldScreen

    draw_colors(acColors, nRow, nWidth)

    SAVE SCREEN TO cScreen

    nRow += 5
    display_box(nRow, nCol, nWidth)

    DO WHILE .T.

        nKey = Inkey(0)

        RESTORE SCREEN FROM cScreen

        DO CASE
            CASE nKey == K_ESC
                EXIT
            CASE nKey == K_DOWN
                IF nRow <= 2 * (Window():get_top() + 1) + 1 + nLength * 4
                    nRow += 2
                    ++nIndexRow
                ELSE
                    nRow := 2 * (Window():get_top() + 3) + 1
                    nIndexRow := 1
                ENDIF
            CASE nKey == K_UP
                IF nRow > 2 * (Window():get_top + 3) + 1
                    nRow -= 2
                    --nIndexRow
                ELSE
                    nRow := 2 * (Window():get_top() + 1) + 3 + nLength * 4
                    nIndexRow := 2 * nLength
                ENDIF
            CASE nKey == K_LEFT
                IF nCol > 2 * nWidth
                    nCol -= 2 * nWidth
                    --nIndexCol
                ELSE
                    nCol := nWidth * nLength * 2
                    nIndexCol := nLength
                ENDIF
            CASE nKey == K_RIGHT
                IF nCol < nWidth * nLength * 2
                    nCol += 2 * nWidth
                    ++nIndexCol
                ELSE
                    nCol := 2 * nWidth
                    nIndexCol := 1
                ENDIF
            CASE nKey == K_CTRL_DOWN .OR. nKey == K_PGDN
                nIndexRow := 2 * nLength
                nRow := 2 * (Window():get_top() + 1) + 3 + nLength * 4
            CASE nKey == K_CTRL_UP .OR. nKey == K_PGUP
                nIndexRow := 1
                nRow := 2 * (Window():get_top() + 3) + 1
            CASE nKey == K_CTRL_LEFT
                nIndexCol := 1
                nCol := 2 * nWidth
            CASE nKey == K_CTRL_RIGHT 
                nIndexCol := nLength
                nCol := nWidth * nLength * 2
            CASE nKey == K_ENTER
                lSave := .T.
                EXIT
        ENDCASE

        display_box(nRow, nCol, nWidth)
    ENDDO

    IF lSave
        IF nIndexRow > nLength
            nIndexRow -= nLength
            lPlus := .T.
        ENDIF
    ENDIF

    RESTORE SCREEN FROM cOldScreen

RETURN IF(lSave, acColors[nIndexRow] + IF(lPlus, '+', '') + '/' + acColors[nIndexCol], '')

STATIC PROCEDURE draw_colors(acColors, nRow, nWidth)

    LOCAL lPlus := .F.
    LOCAL cColor
    LOCAL i, j

    DispBegin()

    FOR i := 1 TO Len(acColors)
        @ nRow * 2, 2 * i * nWidth SAY PadC(acColors[i], nWidth) COLOR 'R'
    NEXT

    DO WHILE .T.
        FOR i := 1 TO Len(acColors)

            IF lPlus
                @ (Len(acColors) + i + nRow) * 2, 1 SAY PadC(acColors[i] + '+', nWidth) COLOR 'R'
            ELSE
                @ (i + nRow) * 2, 1 SAY PadC(acColors[i], nWidth) COLOR 'R'
            ENDIF

            FOR j := 1 TO Len(acColors)
                cColor := acColors[i] + IF(lPlus, '+', '') + '/' + acColors[j]

                IF lPlus
                    @ (Len(acColors) + i + nRow) * 2, 2 * j * nWidth SAY PadC(cColor, nWidth) COLOR acColors[i] + '+/' + acColors[j]
                ELSE
                    @ (i + nRow) * 2, 2 * j * nWidth SAY PadC(cColor, nWidth) COLOR acColors[i] + '/' + acColors[j]
                ENDIF
            NEXT
        NEXT

        IF lPlus
            EXIT
        ELSE
            lPlus := .T.
        ENDIF
    ENDDO

    DispEnd()

RETURN

STATIC PROCEDURE display_box(nRow, nCol, nWidth)

    @ nRow, nCol - 1, nRow + 2, nCol + nWidth BOX B_SINGLE

RETURN
