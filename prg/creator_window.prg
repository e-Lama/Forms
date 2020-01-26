#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_window INHERIT Creator

EXPORTED:

    METHOD edit_form()

ENDCLASS LOCK

METHOD edit_form() CLASS Creator_window

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorWindowHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorWindowFooter'))
    LOCAL lActiveUpperLeftCorner := .T.
    LOCAL lFinish := .F.
    LOCAL lSave := .F.
    LOCAL nOldWindow
    LOCAL cScreen
    LOCAL nKey

    Window():refresh_header()
    Window():refresh_footer()

    SAVE SCREEN TO cScreen

    ::set_type(OBJECT_WINDOW)
    ::clear_window_flag()
    ::make_form_array()

    DO WHILE !lFinish

        nOldWindow := WSelect()
        WSelect(0)
        RESTORE SCREEN FROM cScreen 
        WSelect(nOldWindow)

        ::display_form()

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_ALT_A
                lActiveUpperLeftCorner := !lActiveUpperLeftCorner
            CASE nKey == K_UP
                IF lActiveUpperLeftCorner 
                    IF ::get_value(N_TOP_WN) - 1 < ::get_value(N_BOTTOM_WN) .AND. ::get_value(N_TOP_WN) - 1 >= Window():get_top()
                        ::decrement(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_WN) < ::get_value(N_BOTTOM_WN) - 1 .AND. ::get_value(N_BOTTOM_WN) - 1 >= Window():get_top()
                        ::decrement(N_BOTTOM_WN)
                    ENDIF
                ENDIF    
            CASE nKey == K_LEFT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_WN) - 1 < ::get_value(N_RIGHT_WN) .AND. ::get_value(N_LEFT_WN) - 1 >= Window():get_left()
                        ::decrement(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_WN) < ::get_value(N_RIGHT_WN) - 1 .AND. ::get_value(N_RIGHT_WN) - 1 >= Window():get_left()
                        ::decrement(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_DOWN
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_TOP_WN) + 1 < ::get_value(N_BOTTOM_WN) .AND. ::get_value(N_TOP_WN) + 1 <= Window():get_bottom()
                        ::increment(N_TOP_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_TOP_WN) < ::get_value(N_BOTTOM_WN) + 1 .AND. ::get_value(N_BOTTOM_WN) + 1 <= Window():get_bottom()
                        ::increment(N_BOTTOM_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_RIGHT
                IF lActiveUpperLeftCorner
                    IF ::get_value(N_LEFT_WN) + 1 < ::get_value(N_RIGHT_WN) .AND. ::get_value(N_LEFT_WN) + 1 <= Window():get_right()
                        ::increment(N_LEFT_WN)
                    ENDIF
                ELSE
                    IF ::get_value(N_LEFT_WN) < ::get_value(N_RIGHT_WN) + 1 .AND. ::get_value(N_RIGHT_WN) + 1 <= Window():get_right()
                        ::increment(N_RIGHT_WN)
                    ENDIF
                ENDIF
            CASE nKey == K_ENTER
                ::form_fast_edit(WRow(), WCol(), WLastRow(), WLastCol(), cScreen)
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave'))
                        IF ::save_form()
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
    Window():refresh_header()
    Window():refresh_footer()

RETURN lSave
