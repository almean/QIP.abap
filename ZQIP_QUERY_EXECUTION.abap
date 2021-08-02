*&---------------------------------------------------------------------*
*& Report  ZQIP_QUERY_EXECUTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT  ZQIP_QUERY_EXECUTION.

INCLUDE ZQIP_QUERY_EXECUTION_DATA.
INCLUDE ZQIP_QUERY_EXECUTION_EXCEP.

SELECT-OPTIONS p_def FOR <ls_qip_definition>-name.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK fr_per WITH FRAME TITLE text-001.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS rb_per RADIOBUTTON GROUP rb.
    SELECTION-SCREEN COMMENT 5(30) text-c01 FOR FIELD rb_per.
    SELECTION-SCREEN POSITION 33.
    SELECT-OPTIONS p_per FOR lv_fiscper.

  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS rb_rel RADIOBUTTON GROUP rb.
    SELECTION-SCREEN COMMENT 3(30) text-c02 FOR FIELD rb_per.
    SELECTION-SCREEN POSITION 33.
    SELECT-OPTIONS p_rel FOR lv_relat_per NO-EXTENSION.

  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK fr_per.
SELECTION-SCREEN BEGIN OF BLOCK fr_run WITH FRAME TITLE text-002.

  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS rb_cub RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 5(22) text-c03 FOR FIELD rb_cub.
    SELECTION-SCREEN POSITION 30.
    PARAMETERS rb_alv RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 33(30) text-c04 FOR FIELD rb_alv.

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK fr_run.
SELECTION-SCREEN SKIP.
PARAMETERS p_gen TYPE flag AS CHECKBOX.

START-OF-SELECTION.

  SET LOCALE LANGUAGE 'E'.
  SELECT SINGLE zvalue FROM zhip_rep_var
         INTO lv_fiscvarnt
         WHERE iobjnm = '0FISCVARNT'.

  SELECT SINGLE zvalue FROM zhip_rep_var
         INTO lv_cs_dimen
         WHERE iobjnm = '0CS_DIMEN'.

  SELECT * FROM zqip_definition
         INTO TABLE lt_qip_definitions
         WHERE name IN p_def.
  IF sy-subrc = 0.

* Message: No definitions for these criterias were found

  ELSE.

* Message: Starting processing

  ENDIF.
  PERFORM get_dec_sep.
  PERFORM prepare_period_param.
  LOOP AT lt_qip_definitions  ASSIGNING <ls_qip_definition>.

    PERFORM prepare_query_parameters.
    LOOP AT lt_period INTO ls_period.

      PERFORM execute_query.

    ENDLOOP.

  ENDLOOP.

END-OF-SELECTION.

*-----------------------------------------------------
FORM get_dec_sep.

  DATA:
    TESTP(2)   TYPE P,                "Testfeld gepackt
    TESTC(4)   TYPE C.                "Testfeld Character

  MOVE 1 TO TESTP.
  WRITE TESTP TO TESTC DECIMALS 1.
  IF TESTC+1(1) =  ','.
    MOVE ',' TO gv_dec_sep.
  ELSE.
    MOVE '.' TO gv_dec_sep.
  ENDIF.

ENDFORM.

FORM prepare_period_param.

  DATA ls_per LIKE LINE OF p_per.
  DATA ls_period TYPE /bi0/oifiscper.
  DATA lr_month TYPE RANGE OF /bi0/oicalmonth.
  DATA ll_month LIKE LINE OF lr_month.
  DATA lt_month TYPE TABLE OF /bi0/oicalmonth.
  DATA ls_month TYPE /bi0/oicalmonth.

  CLEAR lr_month[].
  IF rb_rel = 'X'. "Fill absolute with relatives

    CONCATENATE sy-datum(4) '0' sy-datum+4(2) INTO ls_period.
    CALL FUNCTION 'RST_TOBJ_SHIFT'
         EXPORTING
                   i_timnm = '0FISCPER'
                   i_timvl = ls_period
                   i_shift = p_rel-low
                   i_fiscvarnt = 'K0'
         IMPORTING
                   e_timvl = p_per-low.
    CALL FUNCTION 'RST_TOBJ_SHIFT'
         EXPORTING
                   i_timnm = '0FISCPER'
                   i_timvl = ls_period
                   i_shift = p_rel-high
                   i_fiscvarnt = 'K0'
         IMPORTING
                   e_timvl = p_per-high.
    p_per-option = p_rel-option.
    p_per-sign = p_rel-sign.
    APPEND p_per.

  ENDIF.
  IF NOT p_per IS INITIAL.

    SELECT fiscper
           FROM /bi0/sfiscper
           INTO TABLE lt_period
           WHERE fiscvarnt = 'K0' "fixed K0 to avoid extra periods c_fiscvarnt
             AND fiscper IN p_per
             AND datafl = 'X'.
    LOOP AT p_per INTO ls_per.

      MOVE-CORRESPONDING ls_per TO ll_month.
      CONCATENATE ls_per-low(4) ls_per-low+5(2) INTO ll_month-low.
      CONCATENATE ls_per-high(4) ls_per-high+5(2) INTO ll_month-high.
      APPEND ll_month TO lr_month.

    ENDLOOP.

  ENDIF.
  IF NOT lr_month IS INITIAL.

    SELECT calmonth
           FROM /bi0/scalmonth
           INTO TABLE lt_month
           WHERE calmonth IN lr_month.
    LOOP AT lt_month INTO ls_month.

      CONCATENATE ls_month(4) '0'
                  ls_month+4(2) INTO ls_period.
      APPEND ls_period TO lt_period.

    ENDLOOP.

  ENDIF.
  SORT lt_period.
  DELETE ADJACENT DUPLICATES FROM lt_period.
  IF lt_period[] IS INITIAL.

    APPEND ls_period TO lt_period.

  ENDIF.

ENDFORM.

FORM process_result USING p_subrc TYPE sysubrc.

  DATA lv_subrc TYPE sysubrc.

  lv_subrc = p_subrc.
  PERFORM set_period_and_currency.
  PERFORM rename_structures.
  PERFORM create_result_cube_version.
  IF lv_subrc = 0.

    PERFORM fill_result_table.

  ENDIF.
  IF lv_subrc = 0 OR lv_subrc = 1.

    IF rb_cub = 'X'.

      PERFORM delete_previous.
      PERFORM save_to_cube.
      MESSAGE I210(/MAP/E) WITH lv_qty_records lv_fiscper.
*   &1 records of dimension &2 were inserted in database
      WRITE: '    Saved', lv_qty_records, 'records. Period ', lv_fiscper3, ' ', lv_fiscyear.
      SKIP.

    ELSE.

      PERFORM show_alv.

    ENDIF.

  ELSE.

    IF NOT sy-msgid IS INITIAL.

*Message: Issues executing query/view of <ls_qip_definition>-name
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WRITE: sy-msgty, sy-msgid, sy-msgno, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.
      SKIP.

    ENDIF.

  ENDIF.

ENDFORM.

FORM show_alv.

  DATA gr_table TYPE REF TO cl_salv_table.

  ASSIGN lt_target_table->* TO FIELD-SYMBOL(<lt_target_table>).

  cl_salv_table=>factory(
       IMPORTING
                r_salv_table   = gr_table
       CHANGING
                t_table        = <lt_target_table> ).

  gr_table->display( ).

ENDFORM.


FORM save_to_cube.

  DATA lv_requid TYPE rsrequnr.
  DATA lt_msg TYPE rsdri_ts_msg.
  DATA ls_msg TYPE rsdri_s_msg.
  DATA ls_rsldtdone TYPE RSLDTDONE.
  DATA lv_selopt TYPE char128.
  DATA ls_period TYPE /bi0/oifiscper.

  FIELD-SYMBOLS <lt_target_table> TYPE STANDARD TABLE.

  ASSIGN lt_target_table->* TO <lt_target_table>.
  CALL FUNCTION 'RSDRI_CUBE_WRITE_PACKAGE'
       EXPORTING
                i_infocube = <ls_qip_definition>-infocube
                i_curr_conversion = rs_c_false
       IMPORTING
                e_requid = lv_requid
                e_ts_msg = lt_msg
                e_records = lv_qty_records
       CHANGING
                c_t_data = <lt_target_table>
       EXCEPTIONS
                infocube_not_found = 1
                illegal_input = 2
                rollback_error = 3
                duplicate_records = 4
                request_locked  = 5
                not_transactional  = 6
                inherited_error = 7
                others = 8.
  LOOP AT lt_msg INTO ls_msg.

    MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
            INTO data(lv_text)
            WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
    WRITE: lv_text, /.

  ENDLOOP.
  LOOP AT lt_msg INTO ls_msg.

    IF ls_msg-msgty = 'E'.

      RETURN.

    ENDIF.

  ENDLOOP.
  lv_selopt = <ls_qip_definition>-name.
  CONCATENATE lv_selopt '(' lv_fiscyear '.' lv_fiscper3 ')' INTO lv_selopt.
  CONCATENATE lv_fiscyear lv_fiscper3 INTO ls_period.
  UPDATE rsseldone SET oltpsource = lv_selopt
                       source = <ls_qip_definition>-name
                       fieldname = 'FISCPER'
                       iobjnm = '0FISCPER'
                       sign = 'I'
                       opt = 'EQ'
                       low = ls_period
                       description = lv_selopt
                       WHERE rnr = lv_requid.
  FREE <lt_target_table>.

ENDFORM.

FORM create_result_cube_version.

  DATA ls_cube TYPE rsd_s_cube.
  DATA lo_structure TYPE REF TO cl_abap_structdescr.
  DATA lo_table TYPE REF TO cl_abap_tabledescr.
  DATA lt_cube_iobj TYPE rsd_t_cube_iobj.
  DATA ls_cube_iobj TYPE rsd_s_cube_iobj.
  DATA lo_iobj TYPE REF TO cl_rsd_iobj.
  DATA ls_iobj TYPE rsd_s_viobj.
  DATA ls_comp TYPE rsdo_s_cmp.
  DATA ls_iobj_cmp_size TYPE tp_iobj_cmp_size.
  DATA lv_size TYPE i.

  CALL FUNCTION 'RSD_CUBE_GET'
       EXPORTING
                i_infocube = <ls_qip_definition>-infocube
       IMPORTING
                e_s_cube = ls_cube
                e_t_cube_iobj = lt_cube_iobj
       EXCEPTIONS
                infocube_not_found = 1
                illegal_input      = 2
                OTHERS             = 4.
  READ TABLE lt_cube_iobj INTO DATA(ls_first_kf)
       WITH KEY iobjtp = 'KYF'.
  IF sy-subrc = 0.

    lv_default_kf = ls_first_kf-iobjnm.

  ELSE.

    CLEAR lv_default_kf.

  ENDIF.
  lo_structure ?= cl_abap_typedescr=>describe_by_name( ls_cube-viewtiobjnm2 ).
  lo_table ?= cl_abap_tabledescr=>create( p_line_type  = lo_structure
                                          p_table_kind = cl_abap_tabledescr=>tablekind_std
                                          p_unique     = abap_false ).
  CREATE DATA lt_target_table TYPE HANDLE lo_table.
  LOOP AT lt_cube_iobj INTO ls_cube_iobj.

      CALL METHOD cl_rsd_iobj=>factory
           EXPORTING
                    i_iobjnm   = ls_cube_iobj-iobjnm
           RECEIVING
                    r_r_iobj   = lo_iobj.
      CALL METHOD lo_iobj->get_info
           EXPORTING
                    i_objvers = 'A'
           IMPORTING
                    e_s_viobj = ls_iobj
                    e_t_cmp = lt_comp.
      APPEND ls_iobj TO lt_iobj.
      IF NOT lt_comp IS INITIAL.

        ls_iobj_cmp_size-iobj = ls_cube_iobj-iobjnm.
        ls_iobj_cmp_size-size = 0.
        LOOP AT lt_comp INTO ls_comp.

          CALL METHOD cl_rsd_iobj=>factory
               EXPORTING
                        i_iobjnm   = ls_comp-iobjcmp
               RECEIVING
                        r_r_iobj   = lo_iobj.
          CALL METHOD lo_iobj->get_info
               EXPORTING
                        i_objvers = 'A'
               IMPORTING
                        e_s_viobj = ls_iobj.
          lv_size = ls_iobj-intlen.
          ls_iobj_cmp_size-size = ls_iobj_cmp_size-size + lv_size.

        ENDLOOP.
        APPEND ls_iobj_cmp_size TO lt_iobj_cmp_size.

      ENDIF.

  ENDLOOP.

ENDFORM.

FORM set_period_and_currency.

  DEFINE find_var.

    CONCATENATE 'VARIABLE_' &1 INTO lv_name.
    READ TABLE lt_texts INTO ls_text
         WITH KEY sym_name = lv_name
                  sym_value_type = 'V'.
    IF sy-subrc <> 0.

      CONCATENATE 'VARVALUE_' &1 INTO lv_name.
      READ TABLE lt_texts INTO ls_text
           WITH KEY sym_name = lv_name
                    sym_value_type = 'V'.

      IF sy-subrc <> 0.

        CLEAR ls_text.

      ENDIF.

    ENDIF.

  END-OF-DEFINITION.

  DATA ls_text TYPE rrws_s_text_symbols.
  DATA lv_name TYPE rrxsymname.
  DATA lv_chavl TYPE rschavl.
  DATA lv_date TYPE sydatum.
  DATA lv_time_type TYPE c. "Fiscal or Natural

  find_var 'HFISPER3'.
  IF NOT ls_text IS INITIAL.

    lv_fiscper3 = ls_text-sym_value.
    lv_time_type = 'F'.

  ENDIF.
  find_var 'HFISYEAR'.
  IF NOT ls_text IS INITIAL.

    lv_fiscyear = ls_text-sym_value.
    lv_time_type = 'F'.

  ENDIF.
  find_var 'ZMES01'.
  IF NOT ls_text IS INITIAL.

    lv_calmonth2 = ls_text-sym_value.
    lv_time_type = 'N'.

  ENDIF.
  find_var '0P_CALYE'.
  IF NOT ls_text IS INITIAL.

    lv_calyear = ls_text-sym_value.
    lv_time_type = 'N'.

  ENDIF.
  find_var 'H_CURTYP'.
  IF NOT ls_text IS INITIAL.

    lv_curtype = ls_text-sym_value.

  ENDIF.
  CONCATENATE lv_fiscyear lv_fiscper3 INTO lv_fiscper.
  CONCATENATE lv_calyear lv_calmonth2 INTO lv_calmonth.
* Align values if one of them is missing
  IF lv_time_type = 'N'.

    CALL FUNCTION 'FISCPER_FROM_CALMONTH_CALC'
         EXPORTING
                  iv_calmonth = lv_calmonth
                  iv_periv = lv_fiscvarnt
         IMPORTING
                  ev_fiscper3 = lv_fiscper3
                  ev_fiscyear = lv_fiscyear
                  ev_fiscper = lv_fiscper.

  ELSEIF lv_time_type = 'F'.

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
         EXPORTING
                  i_gjahr = lv_fiscyear
                  i_periv = lv_fiscvarnt
                  i_poper = lv_fiscper3
         IMPORTING
                  e_date  = lv_date
         EXCEPTIONS
                  input_false = 1
                  t009_notfound = 2
                  t009b_notfound = 3
                  others = 4.

    IF sy-subrc <> 0.
*     Implement suitable error handling here

    ELSE.

      lv_calmonth = lv_date(6).
      lv_calyear = lv_calmonth(4).
      lv_calmonth2 = lv_calmonth+4(2).

    ENDIF.

  ENDIF.

ENDFORM.

FORM display_alv_table.

  DATA o_alv TYPE REF TO cl_salv_table.

  FIELD-SYMBOLS <lt_target_table> TYPE STANDARD TABLE.

  ASSIGN lt_target_table->* TO <lt_target_table>.
  TRY.

    CALL METHOD cl_salv_table=>factory
         IMPORTING
                   r_salv_table = o_alv
         CHANGING
                   t_table = <lt_target_table>.
  CATCH cx_salv_msg.

  ENDTRY.
  o_alv->display( ).

ENDFORM.

FORM load_hierarchy USING pv_hier TYPE rschavl.

    DATA lv_hier_name TYPE rshienm.
    DATA ls_hier_id TYPE rshi_s_rshiedirkey.

    lv_hier_name = pv_hier.
    SELECT SINGLE *
           FROM rshiedir INTO @DATA(ls_hier_type)
           WHERE hienm = @lv_hier_name
             AND objvers = 'A'.
    CHECK sy-subrc = 0.
    ls_hier_id-hieid = ls_hier_type-hieid.
    ls_hier_id-objvers = 'A'.
    CALL FUNCTION 'RSSH_HIERARCHY_READ'
      EXPORTING
        i_rshiedirkey   = ls_hier_id
      IMPORTING
        e_t_rsnodes     = gt_nodes_a_leaves
        e_th_rsinterval = gt_interval.

ENDFORM.

FORM fill_result_table.

  DEFINE fixed_fill.

    ASSIGN COMPONENT &1 OF STRUCTURE <ls_target_table>
           TO <lv_field>.
    IF sy-subrc = 0.

      <lv_field> = &2.

    ENDIF.

  END-OF-DEFINITION.

  DATA lt_infoobject TYPE TABLE OF rsiobjnm.
  DATA lv_infoobject TYPE rsiobjnm.
  DATA lv_iobj_node TYPE rsiobjnm.
  DATA lv_iobj_hier TYPE rsiobjnm.
  DATA lv_iobj_lvl  TYPE rsiobjnm.
  DATA lv_iobj_parent TYPE rsiobjnm.
  DATA lv_root TYPE rstxtlg.
  DATA lv_iobj_root TYPE rsiobjnm.
  DATA lv_root_lvl TYPE rstlevel.
  DATA ls_target_table TYPE REF TO data.
  DATA lv_temp TYPE string.
  DATA lv_valid_content TYPE sysubrc.
  DATA lv_content TYPE rsatrvl.
  DATA lt_content LIKE TABLE OF lv_content.
  DATA ls_iobj_cmp_size TYPE tp_iobj_cmp_size.
  DATA lv_actual_kf TYPE rsiobjnm.
  DATA lt_temp_char TYPE TABLE OF rrws_sx_axis_chars.
  DATA lv_chanm TYPE rschanm.

  DATA lv_value_to TYPE rsatrvl.
  DATA lv_iobj_to TYPE rsiobjnm.

  FIELD-SYMBOLS <ls_axis_data> TYPE rrws_sx_axis_data.
  FIELD-SYMBOLS <ls_set> TYPE rrws_sx_tuple.
  FIELD-SYMBOLS <lt_target_table> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <ls_target_table> TYPE ANY.
  FIELD-SYMBOLS <lv_field> TYPE any.
  FIELD-SYMBOLS <lv_field2> TYPE any.
  FIELD-SYMBOLS <ls_cell> TYPE rrws_s_cell.
  FIELD-SYMBOLS <ls_axis_info> TYPE rrws_sx_axis_info.
  FIELD-SYMBOLS <ls_char> TYPE rrws_sx_axis_chars.
  FIELD-SYMBOLS <ls_attrinm_set> TYPE rrws_s_attributes.
  FIELD-SYMBOLS <ls_attrinm_char> TYPE rrws_s_attrinm.

  ASSIGN lt_target_table->* TO <lt_target_table>.
  CREATE DATA ls_target_table LIKE LINE OF <lt_target_table>.
  ASSIGN ls_target_table->* TO <ls_target_table>.

  READ TABLE lt_axis_info ASSIGNING <ls_axis_info>
       WITH KEY axis = '001'.
  IF sy-subrc = 0.

    lt_temp_char[] = <ls_axis_info>-chars[].

  ENDIF.
  READ TABLE lt_axis_data ASSIGNING <ls_axis_data>
       WITH KEY AXIS = '001'.
  lv_valid_content = 0.
  LOOP AT <ls_axis_data>-set ASSIGNING <ls_set>.

    lv_chanm = <ls_set>-chanm.
* Search and replace for a different infoobject in the caption
    READ TABLE lt_temp_char ASSIGNING FIELD-SYMBOL(<ls_temp_char>)
         WITH KEY chanm = <ls_set>-chanm.
    IF sy-subrc = 0.

      IF <ls_temp_char>-caption CS c_sep_lvl1.

        SPLIT <ls_temp_char>-caption AT c_sep_lvl1
              INTO lv_chanm lv_temp.

      ENDIF.

    ENDIF.
    SPLIT lv_chanm AT c_sep_lvl2 INTO TABLE lt_infoobject.
* Check if the description has a substitution for this infoobject
    LOOP AT lt_infoobject INTO lv_infoobject.

      IF <ls_set>-drillstate = 'H'.

* Special hierarchy
        CLEAR lv_iobj_node.
        CLEAR lv_iobj_hier.
        CLEAR lv_iobj_lvl.
        CLEAR lv_iobj_parent.
        CLEAR lv_root.
        CLEAR lv_iobj_root.
*        SPLIT lv_infoobject AT c_sep_lvl3
        SPLIT <ls_set>-caption AT c_sep_lvl3
              INTO lv_iobj_node lv_iobj_hier lv_iobj_lvl
                   lv_iobj_parent lv_root.
* Hierarchy node
        ASSIGN COMPONENT lv_iobj_node OF STRUCTURE <ls_target_table>
               TO <lv_field>.
        <lv_field> = <ls_set>-chavl_ext.
        IF <ls_set>-niobjnm IS INITIAL.

* It is not a node, but the characteristic
*  It is not going to the cube
          PERFORM conv_routine USING <ls_set>-chanm
                            CHANGING <lv_field>.
          lv_valid_content = 1.

        ELSE.

          PERFORM conv_routine USING lv_iobj_node
                            CHANGING <lv_field>.

        ENDIF.
* Hierarchy name
        ASSIGN COMPONENT lv_iobj_hier OF STRUCTURE <ls_target_table>
               TO <lv_field>.
        <lv_field> = <ls_set>-chavl.
        IF gt_nodes_a_leaves IS INITIAL AND
           gt_interval       IS INITIAL.

          PERFORM load_hierarchy USING <ls_set>-chavl.

        ENDIF.
        PERFORM conv_routine USING lv_iobj_hier
                          CHANGING <lv_field>.

        IF NOT lv_iobj_lvl IS INITIAL.

* Hierarchy node level
          ASSIGN COMPONENT lv_iobj_lvl OF STRUCTURE <ls_target_table>
                 TO <lv_field>.
          <lv_field> = <ls_set>-tlevel.
          PERFORM conv_routine USING lv_iobj_lvl
                            CHANGING <lv_field>.

        ENDIF.
* Hierarchy node parent
        ASSIGN COMPONENT lv_iobj_parent OF STRUCTURE <ls_target_table>
               TO <lv_field>.
        <lv_field> = <ls_set>-chavl_ext.
        IF <ls_set>-niobjnm IS INITIAL.

          PERFORM conv_routine USING <ls_set>-chanm
                            CHANGING <lv_field>.

        ELSE.

          PERFORM conv_routine USING lv_iobj_parent
                            CHANGING <lv_field>.

        ENDIF.
        READ TABLE gt_nodes_a_leaves ASSIGNING FIELD-SYMBOL(<ls_node>)
             WITH KEY nodename = <lv_field>.
        IF sy-subrc <> 0. "It is an interval

          LOOP AT gt_interval ASSIGNING FIELD-SYMBOL(<ls_interval>)
               WHERE leaffrom <= <lv_field> AND leafto >= <lv_field>.

            EXIT.

          ENDLOOP.
          IF sy-subrc = 0.

            READ TABLE gt_nodes_a_leaves ASSIGNING <ls_node>
                 WITH KEY nodeid = <ls_interval>-nodeid.

          ENDIF.

        ENDIF.
        IF sy-subrc = 0.

          READ TABLE gt_nodes_a_leaves ASSIGNING FIELD-SYMBOL(<ls_parent>)
             WITH KEY nodeid = <ls_node>-parentid.
          IF sy-subrc = 0.

            <lv_field> = <ls_parent>-nodename.

          ELSE.

            CLEAR <lv_field>. "To make sure that no trash left in the field

          ENDIF.

        ENDIF.
* Hierarchy root (not the real, but one defined in query)
        IF NOT lv_root IS INITIAL.

          SPLIT lv_root AT '/'
                INTO lv_iobj_root lv_root_lvl.
          PERFORM conv_routine USING lv_iobj_root
                            CHANGING lv_root_lvl.

          IF lv_root_lvl < <ls_set>-tlevel.

            ASSIGN COMPONENT lv_iobj_root OF STRUCTURE <ls_target_table>
                   TO <lv_field>.
            <lv_field> = <ls_set>-chavl_ext.
            IF <ls_set>-niobjnm IS INITIAL.

              PERFORM conv_routine USING <ls_set>-chanm
                                CHANGING <lv_field>.

            ELSE.

              PERFORM conv_routine USING lv_iobj_root
                                CHANGING <lv_field>.

            ENDIF.
            READ TABLE gt_nodes_a_leaves ASSIGNING <ls_node>
                 WITH KEY nodename = <lv_field>.
            IF sy-subrc <> 0. "It is an interval

              LOOP AT gt_interval ASSIGNING <ls_interval>
                   WHERE leaffrom <= <lv_field> AND leafto >= <lv_field>.

                EXIT.

              ENDLOOP.
              IF sy-subrc = 0.

                READ TABLE gt_nodes_a_leaves ASSIGNING <ls_node>
                   WITH KEY nodeid = <ls_interval>-nodeid.

              ENDIF.

            ENDIF.
            IF sy-subrc = 0.

              DO.

                CLEAR <lv_field>. "To make sure that no trash left in the field
                IF <ls_node>-tlevel = lv_root_lvl.

                  <lv_field> = <ls_node>-nodename.
                  EXIT.

                ENDIF.
                READ TABLE gt_nodes_a_leaves ASSIGNING <ls_node>
                     WITH KEY nodeid = <ls_node>-parentid.
                IF sy-subrc <> 0.

                  EXIT.

                ENDIF.

              ENDDO.

            ENDIF.

          ENDIF.

        ENDIF.
        CONTINUE.

      ELSEIF <ls_set>-drillstate IS INITIAL  OR
             <ls_set>-drillstate = 'E' OR
             <ls_set>-drillstate = 'L' OR
             <ls_set>-drillstate = 'C'. "Only direct infoobject must be verified

        lv_content = <ls_set>-chavl.

      ELSE.

* Let only technical content
        IF 1 = 1. "<ls_set>-caption CS c_sep_lvl1.

          SPLIT <ls_set>-caption AT c_sep_lvl1 INTO lv_content lv_temp.
          SPLIT lv_content AT c_sep_lvl2 INTO TABLE lt_content." <ls_set>-caption.
          READ TABLE lt_content INTO lv_content INDEX sy-tabix.
          IF sy-subrc = 0.

            IF lv_infoobject = '1KYFNM'. "Key Figure definition

              lv_actual_kf = lv_content.
              CLEAR lv_content.

            ENDIF.

          ELSE.

            CLEAR lv_content.

          ENDIF.

        ELSE.

          lv_valid_content = 1.

        ENDIF.

      ENDIF.
      IF lv_content = 'SUMME'.

        lv_content = c_aggregated.

      ELSE.

* Verify compounding
        IF <ls_set>-drillstate IS INITIAL OR
           <ls_set>-drillstate = 'E' OR
           <ls_set>-drillstate = 'L' OR
           <ls_set>-drillstate = 'C'. "Only direct infoobject must be verified

          READ TABLE lt_iobj_cmp_size INTO ls_iobj_cmp_size
               WITH KEY iobj = lv_infoobject.
          IF sy-subrc = 0.

            lv_content = lv_content+ls_iobj_cmp_size-size.

          ENDIF.

        ENDIF.

      ENDIF.
      ASSIGN COMPONENT lv_infoobject OF STRUCTURE <ls_target_table>
             TO <lv_field>.
      IF sy-subrc = 0.

        <lv_field> = lv_content.
        PERFORM conv_routine USING lv_infoobject
                          CHANGING <lv_field>.
        IF lv_content = '#'. "Clear not assigned

          CLEAR <lv_field>.

        ENDIF.

      ELSE.

* This infoobject does not exist in the target cube
*   Only aggregated record must be accepted.
        IF lv_content <> c_aggregated.

          lv_valid_content = 1.

        ENDIF.

      ENDIF.
      PERFORM process_exception USING lv_infoobject lv_content
                                CHANGING lv_iobj_to lv_value_to.
      IF NOT lv_iobj_to IS INITIAL.

        ASSIGN COMPONENT lv_iobj_to OF STRUCTURE <ls_target_table>
               TO <lv_field>.
        IF sy-subrc = 0.

          <lv_field> = lv_value_to.
          PERFORM conv_routine USING lv_iobj_to
                            CHANGING <lv_field>.
          IF lv_content = '#'. "Clear not assigned

            CLEAR <lv_field>.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.
* atributes
    IF NOT <ls_set>-attributes IS INITIAL.

      IF NOT <ls_axis_info> IS ASSIGNED.

        READ TABLE lt_axis_info ASSIGNING <ls_axis_info>
              WITH KEY axis = '001'.

      ENDIF.
      READ TABLE <ls_axis_info>-chars ASSIGNING <ls_char>
           WITH KEY chanm = <ls_set>-chanm.
      LOOP AT <ls_set>-attributes ASSIGNING <ls_attrinm_set>.

        READ TABLE <ls_char>-attrinm ASSIGNING <ls_attrinm_char>
             WITH KEY attrinm = <ls_attrinm_set>-attrinm.
        SPLIT <ls_attrinm_char>-caption AT c_sep_lvl2 INTO TABLE lt_infoobject.
        LOOP AT lt_infoobject INTO lv_infoobject.

          ASSIGN COMPONENT lv_infoobject OF STRUCTURE <ls_target_table>
                 TO <lv_field>.
          IF sy-subrc = 0.

            CONDENSE <ls_attrinm_set>-attrivl.
            <lv_field> = <ls_attrinm_set>-attrivl.
            PERFORM conv_routine USING lv_infoobject
                               CHANGING <lv_field>.
            IF <ls_attrinm_set>-attrivl = '#'. "Clear not assigned

              CLEAR <lv_field>.

            ENDIF.

          ENDIF.
          PERFORM process_exception USING lv_infoobject <ls_attrinm_set>-attrivl
                                    CHANGING lv_iobj_to lv_value_to.
          IF NOT lv_iobj_to IS INITIAL.

            ASSIGN COMPONENT lv_iobj_to OF STRUCTURE <ls_target_table>
                   TO <lv_field>.
            IF sy-subrc = 0.

              CONDENSE lv_value_to.
              <lv_field> = lv_value_to.
              PERFORM conv_routine USING lv_iobj_to
                                 CHANGING <lv_field>.
              IF lv_value_to = '#'. "Clear not assigned

                CLEAR <lv_field>.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDIF.
    AT END OF tuple_ordinal.

      IF lv_actual_kf IS INITIAL.

        lv_actual_kf = lv_default_kf.

      ENDIF.
      fixed_fill 'HRELEASE' 'LASER'.
      fixed_fill '0CS_DIMEN' lv_cs_dimen.
      fixed_fill '0FISCVARNT' lv_fiscvarnt.
      fixed_fill '0FISCPER3' lv_fiscper3.
      fixed_fill '0FISCYEAR' lv_fiscyear.
      fixed_fill '0FISCPER' lv_fiscper.
      fixed_fill '0CALYEAR' lv_calyear.
      fixed_fill '0CALMONTH' lv_calmonth.
      fixed_fill '0CALMONTH2' lv_calmonth2.
      fixed_fill '0CURTYPE' lv_curtype.
      ASSIGN COMPONENT 'HSEGMENT' OF STRUCTURE <ls_target_table>
                 TO <lv_field>.
      IF sy-subrc = 0.

        ASSIGN COMPONENT 'HSUBSEGM' OF STRUCTURE <ls_target_table>
                   TO <lv_field2>.
        IF sy-subrc = 0.

          fixed_fill 'HSEGMENT' <lv_field2>(1).

        ENDIF.

      ENDIF.
*Key figure
      READ TABLE lt_cells ASSIGNING <ls_cell>
           WITH KEY cell_ordinal = <ls_set>-tuple_ordinal
           BINARY SEARCH.
      IF sy-subrc = 0.

        ASSIGN COMPONENT lv_actual_kf OF STRUCTURE <ls_target_table>
               TO <lv_field>.
        REPLACE ALL OCCURRENCES OF '.' IN <ls_cell>-value WITH gv_dec_sep.
        REPLACE ALL OCCURRENCES OF ',' IN <ls_cell>-value WITH gv_dec_sep.
        CALL FUNCTION 'CHAR_FLTP_CONVERSION'
             EXPORTING
                       string = <ls_cell>-value
             IMPORTING
                       flstr = <lv_field>.

      ENDIF.
      IF lv_valid_content = 0.

        APPEND <ls_target_table> TO <lt_target_table>.

      ENDIF.
      CLEAR <ls_target_table>.
      lv_valid_content = 0.

    ENDAT.

  ENDLOOP.

ENDFORM.

FORM conv_routine USING p_iobj TYPE rsiobjnm
                  CHANGING p_val TYPE ANY.

  DATA lv_chanm TYPE rschanm.
  DATA lv_chavlext TYPE rschavlext.
  DATA lv_chavlint TYPE rschavl.

  lv_chanm = p_iobj.
  lv_chavlext = p_val.

  CALL FUNCTION 'RSBCT_CHAVL_EX_IN_CONVERT'
    EXPORTING
      i_chanm              = lv_chanm
      i_chavlext           = lv_chavlext
   IMPORTING
      E_CHAVLINT           = lv_chavlint.

  p_val = lv_chavlint.
*  DATA ls_iobj TYPE rsd_s_viobj.
*  DATA lv_funcname TYPE funcname.
*
*  READ TABLE lt_iobj INTO ls_iobj
*       WITH KEY iobjnm = p_iobj.
*  IF NOT ls_iobj-convexit IS INITIAL.
*
*    lv_funcname = 'CONVERSION_EXIT_&1_INPUT'.
*    REPLACE '&1' WITH ls_iobj-convexit INTO lv_funcname.
*    CALL FUNCTION lv_funcname
*         EXPORTING
*                   input = p_val
*         IMPORTING
*                   output = p_val.
*
*  ENDIF.

ENDFORM.

FORM rename_structures.

  DATA ls_tuple TYPE rrws_sx_tuple.
  DATA ls_char TYPE rrws_sx_axis_chars.
  DATA lt_temp_char TYPE TABLE OF rrws_sx_axis_chars.
  DATA lr_chanm TYPE RANGE OF rschanm.
  DATA ll_chanm LIKE LINE OF lr_chanm.
  DATA ls_new_set TYPE rrws_sx_tuple.
  DATA lt_new_set TYPE rrws_tx_set.
  DATA lt_cube_iobj TYPE rsd_t_cube_iobj.
  DATA lv_OK TYPE c.
  FIELD-SYMBOLS <ls_axis_info> TYPE rrws_sx_axis_info.
  FIELD-SYMBOLS <ls_axis_char> TYPE rrws_sx_axis_chars.
  FIELD-SYMBOLS <ls_axis_data> TYPE rrws_sx_axis_data.
  FIELD-SYMBOLS <ls_attrinm> TYPE rrws_s_attrinm.
  FIELD-SYMBOLS <ls_set> TYPE rrws_sx_tuple.

  READ TABLE lt_axis_info ASSIGNING <ls_axis_info>
       WITH KEY axis = '001'.
  IF sy-subrc <> 0.

* Message: no rows structure in the query/view
    EXIT.

  ENDIF.
  lt_temp_char[] = <ls_axis_info>-chars[].
  LOOP AT lt_temp_char[] ASSIGNING <ls_axis_char>
       WHERE chatyp = 0.

    ls_tuple-chanm = <ls_axis_char>-caption.
    ls_tuple-drillstate = 'X'. "Just to mark this as from a structure
    ls_char = <ls_axis_char>.
    <ls_axis_char>-chanm = ls_tuple-chanm.
    READ TABLE lt_axis_data ASSIGNING <ls_axis_data>
         WITH KEY axis = '001'.
*   Change from that structure technical id to the equivalent
*    infoobject HPI
    MODIFY <ls_axis_data>-set FROM ls_tuple
           TRANSPORTING chanm drillstate
           WHERE chanm = ls_char-chanm.

  ENDLOOP.
* Changes for process hierarchy
  LOOP AT lt_temp_char[] ASSIGNING <ls_axis_char>
       WHERE NOT hienm IS INITIAL
         AND chatyp = 1.

* This hierarchy has the special process for hierarchy?
    IF <ls_axis_char>-caption CS '@'.

*      ls_tuple-chanm   = <ls_axis_char>-caption.
      ls_tuple-caption =  <ls_axis_char>-caption. "This because chanm is not enough
      ls_tuple-drillstate = 'H'. "Just to mark this as special hierarchy
      ls_tuple-chavl = <ls_axis_char>-hienm.
      ls_char = <ls_axis_char>.
      <ls_axis_char>-chanm = ls_tuple-chanm.
      READ TABLE lt_axis_data ASSIGNING <ls_axis_data>
           WITH KEY axis = '001'.
*     Change from that structure technical id to the equivalent
*      infoobject HPI
      MODIFY <ls_axis_data>-set FROM ls_tuple
             TRANSPORTING chavl drillstate caption "chanm
             WHERE chanm = ls_char-chanm.

    ENDIF.

  ENDLOOP..
* Changes for navigational attribute, get caption
  LOOP AT lt_temp_char[] ASSIGNING <ls_axis_char>
       WHERE chanm CS '__'
         AND chatyp = 1.

    ls_tuple-chanm = <ls_axis_char>-caption.
    ls_char = <ls_axis_char>.
    <ls_axis_char>-chanm = ls_tuple-chanm.
    READ TABLE lt_axis_data ASSIGNING <ls_axis_data>
         WITH KEY axis = '001'.
*   Change from that structure technical id to the equivalent
*    infoobject HPI
    MODIFY <ls_axis_data>-set FROM ls_tuple
           TRANSPORTING chanm "drillstate
           WHERE chanm = ls_char-chanm.

  ENDLOOP.

ENDFORM.

FORM prepare_query_parameters.

  DATA lt_string TYPE tdline OCCURS 0 WITH HEADER LINE.
  DATA ls_parameter TYPE w3query.

  CLEAR lt_parameters.
  SPLIT <ls_qip_definition>-parameters AT '&' INTO TABLE lt_string.
  LOOP AT lt_string.

    SPLIT lt_string AT '='
          INTO ls_parameter-name
               ls_parameter-value.
    APPEND ls_parameter TO lt_parameters.

  ENDLOOP.


ENDFORM.

FORM generate_report USING ptype TYPE RSZCOMPTP.

* Regenerate to make sure that the query is corrected
  SUBMIT RSR_GEN_DIRECT_ALL_QUERIES
         WITH i_comptp = ptype
         WITH i_compid = <ls_qip_definition>-compid
         EXPORTING LIST TO MEMORY
         AND RETURN.
  CLEAR p_gen.

ENDFORM.

FORM execute_query.

  PERFORM change_per_param USING ls_period.
  PERFORM check_gen.
  MESSAGE I115(/SOMO/JOBMON) WITH <ls_qip_definition>-name sy-datum sy-uzeit.
* &1 was started on &2 at &3
  WRITE: 'With Parameters'.
  SKIP.
  LOOP AT lt_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).

    WRITE: <ls_parameter>-name, ' -> ', <ls_parameter>-value.
    SKIP.

  ENDLOOP.
  WRITE: 'End Paramenters'.
  SKIP.
  WRITE: 'Running at',
         sy-datum, ' ', sy-uzeit, ' ', <ls_qip_definition>-name.
  SKIP.
  PERFORM get_query_data.
  PERFORM process_result USING sy-subrc.

ENDFORM.

FORM delete_previous.

  SELECT rnr FROM rsiccont
         INTO TABLE @DATA(lt_rnr_target)
         WHERE icube = @<ls_qip_definition>-infocube.

  IF NOT lt_rnr_target IS INITIAL.

    SELECT * FROM rsseldone
         INTO TABLE @DATA(lt_del_req)
         FOR ALL ENTRIES IN @lt_rnr_target
         WHERE rnr = @lt_rnr_target-rnr.
    DELETE lt_del_req WHERE NOT ( fieldname = 'FISCPER' AND
                                source = <ls_qip_definition>-name AND
                                low = ls_period ).
    DELETE ADJACENT DUPLICATES FROM lt_del_req COMPARING rnr.
    LOOP AT lt_del_req INTO DATA(ls_del_req).

        PERFORM add_to_lock USING  ls_del_req-rnr.
        CALL FUNCTION 'RSSM_DELETE_REQUEST'
          EXPORTING
                   request  = ls_del_req-rnr
                   infocube = <ls_qip_definition>-infocube.
        PERFORM check_lock.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM check_lock.

  DATA l_subrc TYPE sysubrc.
  DATA t_enq TYPE TABLE OF seqg3.
  DATA e_flag TYPE sytabix.
  DATA l_ok TYPE c.

  DO.

    l_ok = 'X'.
    LOOP AT lt_object_lock assigning field-symbol(<ls_object_lock>).

      CALL FUNCTION 'ENQUEUE_READ'
           EXPORTING
                     garg   = <ls_object_lock>
                     guname = '*'
           IMPORTING
                     number = e_flag
                     subrc  = l_subrc
           TABLES
                     enq = t_enq
           EXCEPTIONS
                     communication_failure       = 1
                     system_failure              = 2
                     others                      = 3.
      IF e_flag <> 0.

        CLEAR l_ok.
        EXIT.

      ENDIF.

    ENDLOOP.
    IF l_ok = 'X'.

      EXIT.

    ELSE.

      WAIT UP TO 1 SECONDS.

    ENDIF.

  ENDDO.

ENDFORM.

FORM add_to_lock USING p_object.

  DATA ls_object_lock TYPE eqegraarg.
  CONCATENATE '*' p_object '*' INTO ls_object_lock.
  APPEND ls_object_lock TO lt_object_lock.

ENDFORM.

FORM change_per_param USING p_fiscper TYPE /bi0/oifiscper.

  DATA ls_parameter TYPE w3query.
  FIELD-SYMBOLS <ls_parameter> TYPE w3query.
  IF NOT p_fiscper IS INITIAL.

* FISCAL PERIOD
    lv_fiscper3 = p_fiscper+4(3).
    lv_fiscyear = p_fiscper(4).
    READ TABLE lt_parameters INTO ls_parameter
         WITH KEY value = 'HFISYEAR'.
    IF sy-subrc = 0.

      REPLACE 'VAR_NAME' WITH 'VAR_VALUE_EXT' INTO ls_parameter-name.
      READ TABLE lt_parameters ASSIGNING <ls_parameter>
           WITH KEY name = ls_parameter-name.
      <ls_parameter>-value = lv_fiscyear.

    ENDIF.
    READ TABLE lt_parameters INTO ls_parameter
         WITH KEY value = 'HFISPER3'.
    IF sy-subrc = 0.

      REPLACE 'VAR_NAME' WITH 'VAR_VALUE_EXT' INTO ls_parameter-name.
      READ TABLE lt_parameters ASSIGNING <ls_parameter>
           WITH KEY name = ls_parameter-name.
      <ls_parameter>-value = lv_fiscper3.

    ENDIF.

* CALENDAR MONTH
    READ TABLE lt_parameters INTO ls_parameter
         WITH KEY value = '0P_CALYE'.
    IF sy-subrc = 0.

      REPLACE 'VAR_NAME' WITH 'VAR_VALUE_EXT' INTO ls_parameter-name.
      READ TABLE lt_parameters ASSIGNING <ls_parameter>
           WITH KEY name = ls_parameter-name.
      <ls_parameter>-value = lv_fiscyear.

    ENDIF.
    READ TABLE lt_parameters INTO ls_parameter
         WITH KEY value = 'ZMES01'.
    IF sy-subrc = 0.

      REPLACE 'VAR_NAME' WITH 'VAR_VALUE_EXT' INTO ls_parameter-name.
      READ TABLE lt_parameters ASSIGNING <ls_parameter>
           WITH KEY name = ls_parameter-name.
      <ls_parameter>-value = lv_fiscper3+1(2).

    ENDIF.

  ENDIF.

ENDFORM.

FORM check_gen.

  IF p_gen = 'X'.

    IF <ls_qip_definition>-type = 'Q'.

      PERFORM generate_report USING 'REP'.

    ELSE.

      PERFORM generate_report USING 'QVW'.

    ENDIF.

  ENDIF.

ENDFORM.

FORM get_query_data.

  DATA lv_query TYPE rszcompid.
  DATA lv_view TYPE rszviewid.

  CLEAR lt_axis_info[].
  CLEAR lt_cells[].
  CLEAR lt_axis_data[].
  CLEAR lt_texts[].
  IF <ls_qip_definition>-type = 'Q'.

    lv_query = <ls_qip_definition>-compid.
    CALL FUNCTION 'RRW3_GET_QUERY_VIEW_DATA'
         EXPORTING
                  i_query = lv_query
                  i_t_parameter = lt_parameters
         IMPORTING
                  e_axis_info = lt_axis_info
                  e_cell_data = lt_cells
                  e_axis_data = lt_axis_data
                  e_txt_symbols = lt_texts
         EXCEPTIONS
                  no_applicable_data = 1
                  invalid_variable_values = 2
                  no_authority = 3
                  abort = 4
                  invalid_input = 5
                  invalid_view = 6
                  OTHERS = 7.

  ELSE.

    lv_view = <ls_qip_definition>-compid.
    CALL FUNCTION 'RRW3_GET_QUERY_VIEW_DATA'
         EXPORTING
                  i_view_id = lv_view
                  i_t_parameter = lt_parameters
         IMPORTING
                  e_axis_info = lt_axis_info
                  e_cell_data = lt_cells
                  e_axis_data = lt_axis_data
                  e_txt_symbols = lt_texts
         EXCEPTIONS
                  no_applicable_data = 1
                  invalid_variable_values = 2
                  no_authority = 3
                  abort = 4
                  invalid_input = 5
                  invalid_view = 6
                  OTHERS = 7.

  ENDIF.

ENDFORM.
