#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_window INHERIT Creator

EXPORTED:

    METHOD edit_form(xFormCode, xGetPos)

HIDDEN:

    METHOD __refresh_and_display(xFormCodeWithoutWindow, cScreen, lPassedArguments)

ENDCLASS LOCK

METHOD __refresh_and_display(xFormCodeWithoutWindow, cScreen, lPassedArguments) CLASS Creator_window

    MEMVAR GETLIST

    LOCAL nOldWindow := WSelect()

    WSelect(0)

    DispBegin()

    RESTORE SCREEN FROM cScreen 
    WSelect(nOldWindow)

    ::_display_form()

    IF lPassedArguments

        CLEAR GETS

        IF !prepare_form(xFormCodeWithoutWindow)
            Inform(Parser():log(''))
        ENDIF

    ENDIF

    DispEnd()

RETURN NIL

METHOD edit_form(xFormCode, xGetPos) CLASS Creator_window

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorWindowHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorWindowFooter'))
    LOCAL lPassedArguments := (PCount() != 0)
    LOCAL lActiveUpperLeftCorner := .T.
    LOCAL lFinish := .F.
    LOCAL lSave := .F.
    LOCAL nOldWindow
    LOCAL nMouseRow
    LOCAL nMouseCol
    LOCAL lRefresh
    LOCAL xFormCodeWithoutWindow
    LOCAL cScreen
    LOCAL nKey

    IF PCount() != 0
        WClose()
        CLEAR GETS
    ENDIF

    Window():refresh_header_footer()

    SAVE SCREEN TO cScreen

    ::_clear_window_flag()
    ::_set_type(OBJECT_WINDOW)

    IF PCount() != 0
        xFormCodeWithoutWindow := hb_ADel(AClone(xFormCode), 1, .T.)

        IF !prepare_form(xFormCodeWithoutWindow)
            lFinish := .T.
            Inform(Parser():log(''))
        ELSE
            ::_make_form_array(xFormCode)
        ENDIF
    ELSE
        ::_make_form_array()
    ENDIF

    DO WHILE !lFinish

        ::__refresh_and_display(xFormCodeWithoutWindow, cScreen, lPassedArguments)

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_ALT_A
                lActiveUpperLeftCorner := !lActiveUpperLeftCorner
            CASE nKey == K_UP
                IF lActiveUpperLeftCorner 
                    IF ::_get_value(N_TOP_WN) - 1 <= ::_get_value(N_BOTTOM_WN) .AND. ::_get_value(N_TOP_WN) >= Window():get_top()
                        ::_decrement(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_TOP_WN) <= ::_get_value(N_BOTTOM_WN) - 1 .AND. ::_get_value(N_BOTTOM_WN) >= Window():get_top()
                        ::_decrement(N_BOTTOM_WN)
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_LEFT_WN) - 1 <= ::_get_value(N_RIGHT_WN) .AND. ::_get_value(N_LEFT_WN) >= Window():get_left()
                        ::_decrement(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_LEFT_WN) <= ::_get_value(N_RIGHT_WN) - 1 .AND. ::_get_value(N_RIGHT_WN) >= Window():get_left()
                        ::_decrement(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_TOP_WN) + 1 <= ::_get_value(N_BOTTOM_WN) .AND. ::_get_value(N_TOP_WN) <= Window():get_bottom()
                        ::_increment(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_TOP_WN) <= ::_get_value(N_BOTTOM_WN) + 1 .AND. ::_get_value(N_BOTTOM_WN) <= Window():get_bottom()
                        ::_increment(N_BOTTOM_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::_get_value(N_LEFT_WN) + 1 <= ::_get_value(N_RIGHT_WN) .AND. ::_get_value(N_LEFT_WN) <= Window():get_right()
                        ::_increment(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::_get_value(N_LEFT_WN) <= ::_get_value(N_RIGHT_WN) + 1 .AND. ::_get_value(N_RIGHT_WN) <= Window():get_right()
                        ::_increment(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::_form_fast_edit(WRow(), WCol(), WLastRow(), WLastCol(), cScreen, xFormCodeWithoutWindow, xGetPos)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(GETLIST)
                ENDIF
            CASE nKey == K_LBUTTONDOWN

                nKey := 0

                DO WHILE nKey == K_MOUSEMOVE .OR. nKey == 0

                    nKey := Inkey()

                    nOldWindow := WSelect()

                    WSelect(0)

                    nMouseRow := MRow()
                    nMouseCol := MCol()
                    lRefresh := .F.

                    IF lActiveUpperLeftCorner
                        IF nMouseRow > ::_get_value(N_TOP_BOX)
                            IF ::_get_value(N_TOP_WN) + 1 <= ::_get_value(N_BOTTOM_WN) .AND. ::_get_value(N_TOP_WN) <= Window():get_bottom()
                                ::_increment(N_TOP_WN)
                                lRefresh := .T.
                            ENDIF
                        ELSEIF nMouseRow < ::_get_value(N_TOP_WN)
                            IF ::_get_value(N_TOP_WN) - 1 <= ::_get_value(N_BOTTOM_WN).AND. ::_get_value(N_TOP_WN) >= Window():get_top()
                                ::_decrement(N_TOP_WN)
                                lRefresh := .T.
                            ENDIF
                        ENDIF

                        IF nMouseCol > ::_get_value(N_LEFT_WN)
                            IF ::_get_value(N_LEFT_WN) + 1 <= ::_get_value(N_RIGHT_WN) .AND. ::_get_value(N_LEFT_WN) <= Window():get_right()
                                ::_increment(N_LEFT_WN)
                                lRefresh := .T.
                            ENDIF
                        ELSEIF nMouseCol < ::_get_value(N_LEFT_WN)
                            IF ::_get_value(N_LEFT_WN) - 1 <= ::_get_value(N_RIGHT_WN) .AND. ::_get_value(N_LEFT_WN) >= Window():get_left()
                                ::_decrement(N_LEFT_WN)
                                lRefresh := .T.
                            ENDIF
                        ENDIF
                    ELSE
                        IF nMouseRow > ::_get_value(N_BOTTOM_WN)
                            IF ::_get_value(N_TOP_WN) <= ::_get_value(N_BOTTOM_WN) + 1 .AND. ::_get_value(N_BOTTOM_WN) <= Window():get_bottom()
                                ::_increment(N_BOTTOM_WN)
                                lRefresh := .T.
                            ENDIF
                        ELSEIF nMouseRow < ::_get_value(N_BOTTOM_WN)
                            IF ::_get_value(N_TOP_WN) <= ::_get_value(N_BOTTOM_WN) - 1 .AND. ::_get_value(N_BOTTOM_WN) >= Window():get_top()
                                ::_decrement(N_BOTTOM_WN)
                                lRefresh := .T.
                            ENDIF
                        ENDIF

                        IF nMouseCol > ::_get_value(N_RIGHT_WN)
                            IF ::_get_value(N_LEFT_WN) <= ::_get_value(N_RIGHT_WN) + 1 .AND. ::_get_value(N_RIGHT_WN) <= Window():get_right()
                                ::_increment(N_RIGHT_WN)
                                lRefresh := .T.
                            ENDIF
                        ELSEIF nMouseCol < ::_get_value(N_RIGHT_WN)
                            IF ::_get_value(N_LEFT_WN) <= ::_get_value(N_RIGHT_WN) - 1 .AND. ::_get_value(N_RIGHT_WN) >= Window():get_left()
                                ::_decrement(N_RIGHT_WN)
                                lRefresh := .T.
                            ENDIF
                        ENDIF
                    ENDIF

                    WSelect(nOldWindow)

                    IF lRefresh
                        ::__refresh_and_display(xFormCodeWithoutWindow, cScreen, lPassedArguments)
                    ELSE
                      ::_mouse_sleep()
                    ENDIF
                ENDDO
            CASE nKey == K_RBUTTONUP
                ::_display_menu(WRow(), WCol(), WLastRow(), WLastCol(), cScreen, xFormCode, xGetPos, @lActiveUpperLeftCorner, @lFinish, @lSave)
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave'))
                        IF ::_save_form()
                            lFinish := .T.
                            lSave := .T.
                        ENDIF
                    ELSE 
                        lFinish := .T.
                    ENDIF
                ENDIF
        ENDCASE
    ENDDO

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    Window():refresh_header_footer()

RETURN lSave
