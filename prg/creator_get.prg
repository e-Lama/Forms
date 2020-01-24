#include "hbclass.ch"
#include "inkey.ch"

#include "creator.ch"

CLASS Creator_get INHERIT Creator

EXPORTED:

    METHOD edit_form()

ENDCLASS LOCK

METHOD edit_form() CLASS Creator_get

    MEMVAR GETLIST

    LOCAL cOldHeader := Window():header(Config():get_config('CreatorGetHeader'))
    LOCAL cOldFooter := Window():footer(Config():get_config('CreatorGetFooter'))
    LOCAL nTopLimit := IF(WSelect() == 0, Window():get_top(), 0)
    LOCAL nLeftLimit := IF(WSelect() == 0, Window():get_left(), 0)
    LOCAL nBottomLimit := IF(WSelect() == 0, Window():get_bottom(), MaxRow())
    LOCAL nRightLimit := IF(WSelect() == 0, Window():get_right(), MaxCol())
    LOCAL lFinish := .F.
    LOCAL aoWasGetList
    LOCAL lSave 
    LOCAL cScreen
    LOCAL nKey

    Window():refresh_header()
    Window():refresh_footer()

    SAVE SCREEN TO cScreen

    ::set_type(OBJECT_GET)
    ::make_form_array()

    DO WHILE !lFinish

        RESTORE SCREEN FROM cScreen

        ::display_form()
        IF ValType(aoWasGetList) == 'A' .AND. Len(aoWasGetList) != 0 .AND. Len(GETLIST) != 0
            aoWasGetList[Len(aoWasGetList)] := __objClone(GETLIST[Len(GETLIST)])
        ELSE
            aoWasGetList := clone_objects_array(GETLIST)
        ENDIF
        
        GETLIST := ASize(GETLIST, Len(GETLIST) - 1)

        nKey := Inkey(0)

        DO CASE
            CASE nKey == K_UP
                IF ::get_value(N_ROW_GET) - 1 >= nTopLimit
                    ::decrement(N_ROW_GET)
                ENDIF
            CASE nKey == K_LEFT
                IF ::get_value(N_COL_GET) - 1 >= nLeftLimit
                    ::decrement(N_COL_GET)
                ENDIF
            CASE nKey == K_DOWN
                IF ::get_value(N_ROW_GET) + 1 <= nBottomLimit
                    ::increment(N_ROW_GET)
                ENDIF
            CASE nKey == K_RIGHT
                IF ::get_value(N_COL_GET) + 1 <= nRightLimit
                    ::increment(N_COL_GET)
                ENDIF
            CASE nKey == K_ENTER
                ::form_fast_edit(cScreen)
            CASE nKey == K_ALT_ENTER
                IF YesNo(Config():get_config('DoReadOrder'))
                    ReadModal(aoWasGetList)
                ENDIF
            CASE nKey == K_ESC
                IF YesNo(Config():get_config('YesNoBreakEdition'))
                    IF YesNo(Config():get_config('YesNoSave'))
                        IF ::save_form()
                            lFinish := .T.
                            lSave := .T.
                        ELSE
                            lFinish := .F.
                            lSave := .F.
                        ENDIF
                    ELSE 
                        lFinish := .T.
                        lSave := .F.
                    ENDIF
                ENDIF
        ENDCASE
    ENDDO

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    Window():refresh_header()
    Window():refresh_footer()

RETURN lSave
