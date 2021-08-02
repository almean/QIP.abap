*&---------------------------------------------------------------------*
*& Report  ZQIP_DEFINITION_MAINTENANCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT  ZQIP_DEFINITION_MAINTENANCE.

TYPE-POOLS RRX1.
DATA tabname LIKE dd02l-tabname VALUE 'ZQIP_DEFINITION'.

CLASS cl_event_receiver DEFINITION.
  PUBLIC SECTION.

    METHODS:
    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object,
    handle_hotspot_click
            FOR EVENT hotspot_click OF cl_gui_alv_grid
                IMPORTING e_row_id e_column_id es_row_no,
    handle_data_changed
            FOR EVENT data_changed OF cl_gui_alv_grid
                IMPORTING er_data_changed.
  PRIVATE SECTION.
ENDCLASS.                    "CL_EVENT_RECEIVER DEFINITION

CLASS cl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar.
    PERFORM handle_toolbar USING e_object.
  ENDMETHOD.                    "handle_toolbar
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'CCCP'.
        PERFORM update_table.
    ENDCASE.
  ENDMETHOD.                    "handle_user_command
  METHOD handle_hotspot_click .
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no .
  ENDMETHOD .
  METHOD handle_data_changed .
    PERFORM handle_data_changed USING er_data_changed .
  ENDMETHOD.
ENDCLASS.                    "CL_EVENT_RECEIVER IMPLEMENTATION

DATA: lt_dfies TYPE ddfields,
      ls_dfies TYPE dfies.

DATA gs_variant TYPE disvariant.
DATA alv_container TYPE REF TO cl_gui_custom_container.
DATA alv_grid TYPE REF TO cl_gui_alv_grid.
DATA event_receiver TYPE REF TO cl_event_receiver.
DATA lt_exclude TYPE ui_functions.

DATA fieldcat TYPE lvc_t_fcat.
DATA fc             TYPE lvc_s_fcat.
DATA layout TYPE lvc_s_layo.

DATA xoutput LIKE layout-frontend VALUE 'G'.

DATA gv_rec_proc TYPE i.
DATA gv_rec_nonp TYPE i.
DATA gv_rec_dupl TYPE i.

DATA ls_dd03vt TYPE dd03vt.
DATA ls_dd03l TYPE dd03l.

DATA: BEGIN OF ds_where OCCURS 0,
       line(72),
      END OF ds_where.

FIELD-SYMBOLS <fs_itab> TYPE STANDARD TABLE.
FIELD-SYMBOLS <fs_dtab> TYPE STANDARD TABLE.
FIELD-SYMBOLS <fs_xtab> TYPE STANDARD TABLE.
FIELD-SYMBOLS <ld_fld> TYPE ANY.

DEFINE show_alv.
*--- &1, Container
*--- &2, Container ID
*--- &3, ALV Layout
*--- &4, ALV Display Variant
*--- &5, Table to display in ALV
*--- &6, ALV Field Catalouge
*--- &7, ALV Grid ID
*--- Create Object Container
  if &1 is initial.
    create object &1
    exporting
      container_name    = &2.
*--- Create Object ALV Grid assigning container
    create object &7
    exporting
      i_parent          =  &1.
*--- Create Event Receiver
    create object event_receiver.
*--- Assign methods for event handling
    set handler event_receiver->handle_toolbar for &7.
    set handler event_receiver->handle_user_command for &7.
    SET HANDLER event_receiver->handle_hotspot_click FOR &7.
    SET HANDLER event_receiver->handle_data_changed FOR &7.
*    set handler event_receiver->handle_double_click for &7.
*--- Exclude toolbar buttons
    perform exclude_tb_functions changing lt_exclude.
*--- Display ALV Grid Report
    call method &7->set_table_for_first_display
      exporting
        is_layout            = &3
        is_variant           = &4
*        i_save               = 'A'
        it_toolbar_excluding = lt_exclude
      changing
        it_outtab            = &5
        it_fieldcatalog      = &6.
    call method &7->set_ready_for_input
      exporting
        i_ready_for_input = 0.
  else.
    set handler event_receiver->handle_toolbar for &7.
    call method &7->refresh_table_display.
  endif.
*--- Set focus to the active ALV Grid
  call method &7->set_toolbar_interactive.
  call method cl_gui_control=>set_focus
    exporting
      control = &7.
END-OF-DEFINITION.

START-OF-SELECTION.

  DATA lt_parameter TYPE TABLE OF tdline.
  PERFORM alv_set_layout.     "Define ALV Layout
  PERFORM alv_set_fieldcat.   "Create ALV field catalogue for initial display
  PERFORM mydyntable USING  fieldcat. "Create Dynamic tables based on DB table name to be updated
  IF <fs_itab> IS ASSIGNED.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = tabname
      TABLES
        dfies_tab      = lt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
  ENDIF.

  SELECT *
         FROM (tabname)
         INTO CORRESPONDING FIELDS OF TABLE <fs_itab>.
  CALL SCREEN 100.

END-OF-SELECTION.

FORM mydyntable USING lt TYPE lvc_t_fcat.
*-------------- Create Dyn Table From FC
  FIELD-SYMBOLS: <fs_data> TYPE REF TO data.
  FIELD-SYMBOLS: <fs_1> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <fs_tmpd> TYPE REF TO data.
  FIELD-SYMBOLS: <fs_3> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <fs_xhtb> TYPE REF TO data.
  FIELD-SYMBOLS: <fs_7> TYPE STANDARD TABLE.
*------------- Create Reoprting Table
  DATA: lt_data TYPE REF TO data.
  ASSIGN lt_data TO <fs_data>.
  IF <fs_data> IS ASSIGNED.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = lt
      IMPORTING
        ep_table                  = <fs_data>
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
    ASSIGN <fs_data>->* TO <fs_1>.
    IF <fs_1> IS ASSIGNED.
      ASSIGN <fs_1> TO <fs_itab>.
    ENDIF.
  ENDIF.
*------------ Create Deletion Table
  DATA: lt_tmp TYPE REF TO data.
  ASSIGN lt_tmp TO <fs_tmpd>.
  IF <fs_tmpd> IS ASSIGNED.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = lt
      IMPORTING
        ep_table                  = <fs_tmpd>
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
    ASSIGN <fs_tmpd>->* TO <fs_3>.
    IF <fs_3> IS ASSIGNED.
      ASSIGN <fs_3> TO <fs_dtab>.
    ENDIF.
  ENDIF.
*------------ Create Master Table
  DATA: lt_xh TYPE REF TO data.
  ASSIGN lt_xh TO <fs_xhtb>.
  IF <fs_xhtb> IS ASSIGNED.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = lt
      IMPORTING
        ep_table                  = <fs_xhtb>
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
    ASSIGN <fs_xhtb>->* TO <fs_7>.
    IF <fs_7> IS ASSIGNED.
      ASSIGN <fs_7> TO <fs_xtab>.
    ENDIF.
  ENDIF.
ENDFORM. "MYDYNTABLE

FORM alv_set_layout.
*... Display options
*  layout-zebra = 'X'.
*  layout-excp_conds = 'X'.
*  layout-cwidth_opt = ''.
*  layout-frontend  = xoutput.
ENDFORM.                    "alv_set_layout

FORM alv_set_fieldcat .
  REFRESH: fieldcat.
  DATA: v_text(40), lv_index LIKE sy-index.
  DATA: v_h1(40),v_h2(40),v_h3(40),v_h4(40),v_h5(40).
  FIELD-SYMBOLS: <tbnm> TYPE ANY.
  ASSIGN tabname TO <tbnm>.
  IF <tbnm> IS ASSIGNED.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_buffer_active        = ''
        i_structure_name       = <tbnm>
        i_client_never_display = ''
      CHANGING
        ct_fieldcat            = fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2.
    UNASSIGN <tbnm>.
    LOOP AT fieldcat INTO fc.
      SELECT SINGLE * FROM dd03vt
              INTO ls_dd03vt
        WHERE tabname = tabname AND fieldname = fc-fieldname
          AND ddlanguage = sy-langu.
      fc-rollname = ls_dd03vt-rollname.
      fc-checktable = ls_dd03vt-checktable.
      IF fc-key = 'X'.
        CONCATENATE '*' ls_dd03vt-scrtext_s INTO fc-scrtext_s SEPARATED BY space.
        CONCATENATE '*' ls_dd03vt-scrtext_m INTO fc-scrtext_m SEPARATED BY space.
        CONCATENATE '*' ls_dd03vt-scrtext_l INTO fc-scrtext_l SEPARATED BY space.
        CONCATENATE '*' ls_dd03vt-reptext   INTO fc-reptext   SEPARATED BY space.
        CONCATENATE '*' ls_dd03vt-reptext   INTO fc-seltext   SEPARATED BY space.
        MOVE            ls_dd03vt-scrtext_l TO fc-tooltip.
      ELSE.
        MOVE: ls_dd03vt-scrtext_s TO fc-scrtext_s,
              ls_dd03vt-scrtext_m TO fc-scrtext_m,
              ls_dd03vt-scrtext_l TO fc-scrtext_l,
              ls_dd03vt-reptext   TO fc-reptext,
              ls_dd03vt-reptext   TO fc-seltext,
              ls_dd03vt-scrtext_l TO fc-tooltip.
      ENDIF.
      IF fc-checktable IS INITIAL OR fc-checktable = '*'.
        SELECT SINGLE entitytab INTO fc-checktable FROM dd01l WHERE domname = fc-domname.
      ENDIF.
      IF fc-fieldname <> 'MANDT'.
        fc-edit = 'X'.
      ENDIF.
      IF fc-datatype = 'STRG'.

        fc-hotspot = 'X'.
        CLEAR fc-edit.

      ENDIF.
      MODIFY fieldcat FROM fc.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " alv_set_fieldcat

FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.
  DATA ls_exclude TYPE ui_func.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_help.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_detail.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO pt_exclude.
ENDFORM.                    "EXCLUDE_TB_FUNCTIONS

FORM handle_toolbar USING i_object TYPE REF TO cl_alv_event_toolbar_set .
*--- This code adds the Pushbutton for comparison to the ALV Grid toolbar
  DATA ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE 0 TO ls_toolbar-butn_type.
  MOVE 'CCCP' TO ls_toolbar-function.                       "#EC NOTEXT
  MOVE icon_system_save  TO ls_toolbar-icon.
  MOVE 'Save Changed Data'(201) TO ls_toolbar-quickinfo.
  IF sy-ucomm = 'EDIT'.
    MOVE ' ' TO ls_toolbar-disabled.                        "#EC NOTEXT
  ELSE.
    MOVE 'X' TO ls_toolbar-disabled.                        "#EC NOTEXT
  ENDIF.
  APPEND ls_toolbar TO i_object->mt_toolbar.

ENDFORM .                    "handle_toolbar

FORM handle_hotspot_click USING i_row_id TYPE lvc_s_row
          i_column_id TYPE lvc_s_col
          is_row_no TYPE lvc_s_roid.

  DATA lv_answer TYPE c.
  DATA lt_opt TYPE TABLE OF spopli.
  DATA ls_opt TYPE spopli.

  ls_opt-varoption = 'Process as string'.
  APPEND ls_opt TO lt_opt.
  ls_opt-varoption = 'Process as variables'.
  APPEND ls_opt TO lt_opt.
  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
       EXPORTING
                textline1 = 'Choose the way to process'
                titel = 'Way of process'
       IMPORTING
                answer = lv_answer
       TABLES
                t_spopli = lt_opt.
  IF lv_answer = '2'.

    PERFORM hotspot_click_variable USING i_row_id
                                         i_column_id
                                         is_row_no.

  ELSEIF lv_answer = '1'.

    PERFORM hotspot_click_string USING i_row_id
                                       i_column_id
                                       is_row_no.

  ENDIF.

ENDFORM.

FORM hotspot_click_string USING i_row_id TYPE lvc_s_row
          i_column_id TYPE lvc_s_col
          is_row_no TYPE lvc_s_roid.

  DATA lt_string TYPE tdline OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS <lv_field> TYPE string.
  FIELD-SYMBOLS <ls_record> TYPE ANY.

  READ TABLE <fs_itab> ASSIGNING <ls_record> INDEX is_row_no-row_id .
  IF <ls_record> IS ASSIGNED.

    ASSIGN COMPONENT i_column_id-fieldname OF STRUCTURE <ls_record>
           TO <lv_field>.
    SPLIT <lv_field> AT '&' INTO TABLE lt_string.
    CALL FUNCTION 'TERM_CONTROL_EDIT'
         TABLES
               textlines = lt_string
         EXCEPTIONS
                   user_cancelled = 1
                   others = 2.
    IF alv_grid->is_ready_for_input( ) IS INITIAL.

      EXIT.

    ENDIF.
    IF sy-subrc = 0.

      CLEAR <lv_field>.
      LOOP AT lt_string.

        IF <lv_field> IS INITIAL.

          <lv_field> = lt_string.

        ELSE.

          REPLACE 'VAR_ID_' WITH 'VAR_NAME_' INTO <lv_field>.
          CONCATENATE <lv_field> lt_string INTO <lv_field> SEPARATED BY '&'.

        ENDIF.

      ENDLOOP.
      CALL METHOD alv_grid->refresh_table_display.

    ENDIF.

  ENDIF.

ENDFORM .

FORM hotspot_click_variable USING i_row_id TYPE lvc_s_row
          i_column_id TYPE lvc_s_col
          is_row_no TYPE lvc_s_roid.

  DATA l_handle TYPE rsidn4.
  DATA lv_genuniid TYPE rsgenuniid.
  DATA lt_var TYPE TABLE OF rrx_var.
  DATA ls_var TYPE rrx_var.
  DATA lt_string TYPE tdline OCCURS 0 WITH HEADER LINE.
  DATA lt_prptys TYPE rrx1_t_prptys.
  DATA lv_indx TYPE string.
  DATA lv_search TYPE string.
  DATA lv_count TYPE sytabix.
  DATA lv_subrc TYPE sysubrc.
  DATA lv_name TYPE string.
  DATA lv_value TYPE string.
  DATA lo_request TYPE REF TO cl_rsr_request.
  FIELD-SYMBOLS <lv_compid> TYPE rszcompid.
  FIELD-SYMBOLS <lv_var_string> TYPE string.
  FIELD-SYMBOLS <ls_record> TYPE ANY.

  READ TABLE <fs_itab> ASSIGNING <ls_record> INDEX is_row_no-row_id .
  CHECK <ls_record> IS ASSIGNED.

  ASSIGN COMPONENT i_column_id-fieldname OF STRUCTURE <ls_record>
         TO <lv_var_string>.
  SPLIT <lv_var_string> AT '&' INTO TABLE lt_string.
  lv_count = 1.
  DO.

    IF NOT lines( lt_string ) > 0.

      EXIT.

    ENDIF.
    lv_indx = lv_count.
    CONCATENATE '_' lv_indx '=' INTO lv_search.
    CONDENSE lv_search NO-GAPS.
    CLEAR ls_var.
    LOOP AT lt_string.

*Check if it is a valid line. It must have = and _
      IF NOT ( lt_string CS '=' AND lt_string CS '_' ).

        DELETE lt_string.

      ELSEIF lt_string CS lv_search.

        SPLIT lt_string AT '=' INTO lv_name lv_value.
        IF lv_name CS 'VAR_NAME_'.

          ls_var-vnam = lv_value.

        ENDIF.
        IF lv_name CS 'VAR_NODE_IOBJNM_'.

          ls_var-high = lv_value.

        ENDIF.
        IF lv_name CS 'VAR_VALUE_EXT_' OR
           lv_name CS 'VAR_VALUE_LOW_EXT_'.

          ls_var-low_ext = lv_value.

        ENDIF.
        IF lv_name CS 'VAR_VALUE_HIGH_EXT_'.

          ls_var-high_ext = lv_value.

        ENDIF.
        DELETE lt_string.

      ENDIF.

    ENDLOOP.
    IF NOT ls_var-vnam IS INITIAL.

      IF ls_var-sign IS INITIAL.

        ls_var-sign = 'I'.

      ENDIF.
      IF ls_var-opt IS INITIAL.

        ls_var-opt = 'EQ'.

      ENDIF.
      APPEND ls_var TO lt_var.

    ENDIF.
    lv_count = lv_count + 1.

  ENDDO.
  ASSIGN COMPONENT 'COMPID' OF STRUCTURE <ls_record>
         TO <lv_compid>.
  SELECT SINGLE genuniid
         INTO lv_genuniid
         FROM rsrrepdir
         WHERE compid = <lv_compid>
           AND objvers = 'A'.
  CHECK sy-subrc = 0.
  l_handle = cl_rsr_request=>get_new_handle( ).
  CREATE OBJECT lo_request
             EXPORTING i_genuniid = lv_genuniid
                       i_cj_setxx_with_structures = rs_c_true
                       i_handle                   = l_handle
                       i_called_by_analyzer_3x    = 'X'.
  lo_request->variables_set( EXPORTING
                               i_pmode = ' '
                               i_selscrn = 'U'
                               i_statemode = 'U' "'M' 'I' 'K'  'U'
                               i_t_var = lt_var[] ).
  CALL METHOD lo_request->variables_send_abap_dynpro
       EXPORTING
                 i_force = rs_c_true
       EXCEPTIONS
                 bad_value_combination = 1
                 screen_canceled       = 2
                 user_not_authorized   = 3
                 x_message             = 8.
  IF sy-subrc <> 0 AND sy-subrc <> 2. "Error in string. empty the values and rerrun

    CLEAR lt_var[].
    lo_request->variables_set( EXPORTING i_t_var = lt_var[] ).
    CALL METHOD lo_request->variables_send_abap_dynpro
         EXPORTING
                 i_force = rs_c_true
         EXCEPTIONS
                 bad_value_combination = 1
                 screen_canceled       = 2
                 user_not_authorized   = 3
                 x_message             = 8.

  ENDIF.
  lt_var[] = lo_request->n_sx_request-var.
  CHECK lv_subrc = 0.
  CLEAR lt_string[].
  LOOP AT lt_var INTO ls_var.

    lv_indx = sy-tabix.
    CONCATENATE 'VAR_NAME_' lv_indx '=' ls_var-vnam
                INTO lt_string.
    CONDENSE lt_string NO-GAPS.
    APPEND lt_string.
    IF ls_var-high_ext = '0HIER_NODE'.

      CONCATENATE 'VAR_NODE_IOBJNM_' lv_indx '=' ls_var-high_ext
                  INTO lt_string.
      CONDENSE lt_string NO-GAPS.
      APPEND lt_string.
      CONCATENATE 'VAR_VALUE_EXT_' lv_indx '=' ls_var-low_ext
                  INTO lt_string.
      CONDENSE lt_string NO-GAPS.
      APPEND lt_string.

    ELSEIF ls_var-high_ext IS INITIAL.

      CONCATENATE 'VAR_VALUE_EXT_' lv_indx '=' ls_var-low_ext
                  INTO lt_string.
      CONDENSE lt_string NO-GAPS.
      APPEND lt_string.

    ELSE.

      CONCATENATE 'VAR_VALUE_LOW_EXT_' lv_indx '=' ls_var-low_ext
                  INTO lt_string.
      CONDENSE lt_string NO-GAPS.
      APPEND lt_string.
      CONCATENATE 'VAR_VALUE_HIGH_EXT_' lv_indx '=' ls_var-high_ext
                  INTO lt_string.
      CONDENSE lt_string NO-GAPS.
      APPEND lt_string.

    ENDIF.

  ENDLOOP.
  CHECK NOT alv_grid->is_ready_for_input( ) IS INITIAL.
  CLEAR <lv_var_string>.
  LOOP AT lt_string.

    IF <lv_var_string> IS INITIAL.

     <lv_var_string> = lt_string.

    ELSE.

      REPLACE 'VAR_ID_' WITH 'VAR_NAME_' INTO <lv_var_string>.
      CONCATENATE <lv_var_string> lt_string INTO <lv_var_string> SEPARATED BY '&'.

    ENDIF.

  ENDLOOP.
  CALL METHOD alv_grid->refresh_table_display.

ENDFORM .

FORM handle_data_changed USING ir_data_changed
                         TYPE REF TO cl_alv_changed_data_protocol.

  DATA ls_deleted TYPE lvc_s_moce.
  FIELD-SYMBOLS <dline> TYPE ANY.
  LOOP AT ir_data_changed->mt_deleted_rows INTO ls_deleted.

    READ TABLE <fs_itab> ASSIGNING <dline> INDEX ls_deleted-row_id.
    IF sy-subrc = 0.

      APPEND <dline> TO <fs_dtab>.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " handle_data_changed

FORM update_table .
  DATA: l_answer TYPE c.
  DATA: l_tabix LIKE sy-tabix.
  FIELD-SYMBOLS: <dline> TYPE ANY,
                 <iline> TYPE ANY.
  DATA: lv_answer TYPE c.
  DATA: lv_error  TYPE c.
  DATA: lv_count  TYPE i.
  DATA: lv_fld(255) TYPE c.
  IF alv_grid->is_ready_for_input( ) IS INITIAL.

    EXIT.

  ENDIF.
  l_answer = 1.
  IF l_answer = '1'.
*---- Delete all common records from <fs_dtab> comparing <fs_itab>
*---- Now <fs_dtab> will have only deleted or changed records and
*---- Now <fs_itab> will have records which need to updated to table
    alv_grid->check_changed_data( ).
    LOOP AT <fs_itab> ASSIGNING <dline>.
      IF <dline> IS ASSIGNED.
        LOOP AT <fs_dtab> ASSIGNING <iline>.
          l_tabix = sy-tabix.
          IF <iline> IS ASSIGNED AND <dline> IS ASSIGNED.
            IF <dline> = <iline>.
              DELETE <fs_dtab> INDEX l_tabix.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
*--- Delete Records marked for deletion & Change
    IF NOT <fs_dtab>[] IS INITIAL.
      DELETE (tabname) FROM TABLE <fs_dtab>.
    ENDIF.
*--- Update Records Marked for change or creation
    IF NOT <fs_itab>[] IS INITIAL.
      LOOP AT <fs_itab> ASSIGNING <dline>.
        gv_rec_proc = gv_rec_proc + 1.
        REFRESH ds_where.
        LOOP AT fieldcat INTO fc WHERE key = 'X' AND fieldname <> 'MANDT'.
          ASSIGN COMPONENT fc-fieldname OF STRUCTURE <dline> TO <iline>.
          CONDENSE: <iline>, fc-fieldname.
          CLEAR: lv_fld.
          CONCATENATE `'` <iline> `'` INTO lv_fld.
          IF sy-tabix = 2.
            CONCATENATE  '   ( ' fc-fieldname  'EQ' lv_fld ' )' INTO ds_where-line SEPARATED BY space.
            APPEND ds_where.
          ELSEIF sy-tabix > 2.
            CONCATENATE  'AND' '( ' fc-fieldname  'EQ' lv_fld ' )' INTO ds_where-line SEPARATED BY space.
            APPEND ds_where.
          ENDIF.
        ENDLOOP.
        IF <fs_xtab> IS ASSIGNED.

          IF NOT ds_where IS INITIAL.

            SELECT *
                   FROM (tabname)
                   INTO CORRESPONDING FIELDS OF TABLE <fs_xtab>
                   WHERE (ds_where).

          ELSE.

            SELECT *
                   FROM (tabname)
                   INTO CORRESPONDING FIELDS OF TABLE <fs_xtab>.

          ENDIF.
          IF sy-subrc <> 0.
            LOOP AT fieldcat INTO fc WHERE checktable <> ''.
              lv_error = 'X'.
              FIELD-SYMBOLS: <ls_temp> TYPE ANY.
              DATA: dref_table_line TYPE REF TO data.
              CREATE DATA dref_table_line TYPE (fc-checktable).
              ASSIGN dref_table_line->* TO <ls_temp>.
              IF <dline> IS ASSIGNED AND <ls_temp> IS ASSIGNED.
                REFRESH: ds_where.
                ASSIGN COMPONENT fc-fieldname OF STRUCTURE <dline> TO <iline>.
                CLEAR: lv_fld.
                IF <iline> IS ASSIGNED AND NOT <iline> IS INITIAL.
                  SELECT * FROM dd03l INTO ls_dd03l
                         WHERE tabname = fc-checktable AND rollname = fc-rollname
                           AND domname = fc-domname.
                    REFRESH: ds_where.
                    IF fc-checktable <> tabname.
                      CONCATENATE `'` <iline> `'` INTO lv_fld.
                      CONCATENATE  '( ' ls_dd03l-fieldname 'EQ' lv_fld ' )' INTO ds_where-line SEPARATED BY space.
                      APPEND ds_where.
                      IF <iline> IS ASSIGNED AND NOT <iline> IS INITIAL.
                        IF NOT ds_where[] IS INITIAL.
                          SELECT SINGLE * INTO CORRESPONDING FIELDS OF <ls_temp> FROM (fc-checktable) WHERE (ds_where).
                          IF sy-subrc = 0.
                            lv_error = ' '.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDSELECT.
                  IF sy-subrc <> 0 AND fc-checktable <> tabname AND lv_error = 'X'.
                    SELECT * FROM dd03l INTO ls_dd03l
                           WHERE tabname = fc-checktable AND domname = fc-domname.
                      REFRESH: ds_where.
                      IF sy-subrc = 0 AND fc-checktable <> tabname.
                        CONCATENATE `'` <iline> `'` INTO lv_fld.
                        CONCATENATE  '( ' ls_dd03l-fieldname 'EQ' lv_fld ' ).' INTO ds_where-line SEPARATED BY space.
                        APPEND ds_where.
                        IF <iline> IS ASSIGNED AND NOT <iline> IS INITIAL.
                          IF NOT ds_where[] IS INITIAL.
                            SELECT SINGLE * INTO CORRESPONDING FIELDS OF <ls_temp> FROM (fc-checktable) WHERE (ds_where).
                            IF sy-subrc = 0.
                              lv_error = ' '.
                              EXIT.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDSELECT.
                  ENDIF.
                  IF lv_error = 'X' AND fc-checktable <> tabname.
                    lv_count = lv_count + 1.
                    CONDENSE fc-fieldname.
                    WRITE:/ 'Field', fc-fieldname, 'value', <iline> ,'not in table ', fc-checktable.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF <ls_temp> IS ASSIGNED.
                UNASSIGN <ls_temp>.
              ENDIF.
              IF <iline> IS ASSIGNED.
                UNASSIGN <iline>.
              ENDIF.
            ENDLOOP.
            IF lv_count = 0.
              MODIFY (tabname) FROM <dline>.
            ELSEIF lv_count <> 0.
              lv_count = 0.
              WRITE:/ 'Above mentioned errors in this record: ', gv_rec_proc.
              LOOP AT lt_dfies INTO ls_dfies.
                ASSIGN COMPONENT ls_dfies-fieldname OF STRUCTURE <dline> TO <ld_fld>.
                WRITE: <ld_fld>.
              ENDLOOP.
              SKIP.
              DELETE <fs_itab>.
              gv_rec_nonp = gv_rec_nonp + 1.
              lv_error = ''.
            ENDIF.
          ELSE.
            WRITE:/ 'Already Exists,Non Key Fields Updated: ', gv_rec_proc.
            LOOP AT lt_dfies INTO ls_dfies.
              ASSIGN COMPONENT ls_dfies-fieldname OF STRUCTURE <dline> TO <ld_fld>.
              WRITE: <ld_fld>.
            ENDLOOP.
            gv_rec_dupl = gv_rec_dupl + 1.
            MODIFY (tabname) FROM <dline>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  COMMIT WORK AND WAIT.
ENDFORM.                    " update_table

MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'STLI'.
  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.
  show_alv alv_container 'ALV_CONTAINER' layout gs_variant <fs_itab>[] fieldcat[] alv_grid.

ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EDIT'.
      CALL METHOD alv_grid->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
    WHEN 'SAVE'.
      PERFORM update_table.
    WHEN 'DISP'.
      CALL METHOD alv_grid->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      CALL METHOD alv_container->free.
      CLEAR: alv_container.
      FREE : alv_container.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'RW'.
      CALL METHOD alv_container->free.
      CLEAR: alv_container.
      FREE : alv_container.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

FORM init_prptys CHANGING c_t_prptys TYPE rrx1_t_prptys.

  DATA: l_s_global_settings TYPE rrxgblset,
        l_s_prptys          TYPE rrx1_s_prptys.

  CALL FUNCTION 'RRSV_GLOBAL_SETTINGS_GET'
    IMPORTING
      e_s_global_settings = l_s_global_settings.

  l_s_prptys-id    = rrx1_c_prptys_id-percentfrac.
  l_s_prptys-value = l_s_global_settings-percntfrac.
  APPEND l_s_prptys TO c_t_prptys.

  l_s_prptys-id    = rrx1_c_prptys_id-mask_date.
  l_s_prptys-value = l_s_global_settings-mask_date.
  APPEND l_s_prptys TO c_t_prptys.

  l_s_prptys-id    = rrx1_c_prptys_id-mask_time.
  l_s_prptys-value = l_s_global_settings-mask_time.
  APPEND l_s_prptys TO c_t_prptys.

  l_s_prptys-id    = rrx1_c_prptys_id-dcpchar.
  l_s_prptys-value = l_s_global_settings-dcpchar.
  APPEND l_s_prptys TO c_t_prptys.

  l_s_prptys-id    = rrx1_c_prptys_id-signprsnt.
  l_s_prptys-value = l_s_global_settings-signprsnt.
  IF l_s_prptys-value IS INITIAL.
    l_s_prptys-value = '1'.
  ENDIF.
  APPEND l_s_prptys TO c_t_prptys.

  l_s_prptys-id    = rrx1_c_prptys_id-formatted_values.
  l_s_prptys-value = 'X'.
  APPEND l_s_prptys TO c_t_prptys.

ENDFORM.                    "init_prptys
