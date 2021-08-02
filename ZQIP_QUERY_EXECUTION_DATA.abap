*&---------------------------------------------------------------------*
*&  Include           ZQIP_QUERY_EXECUTION_DATA
*&---------------------------------------------------------------------*

TYPE-POOLS rsd.

* Level 1: split infoobject content from the unused part
CONSTANTS c_sep_lvl1(2) TYPE c VALUE '!!'.
* Level 2: split several infoobjects
CONSTANTS c_sep_lvl2 TYPE c VALUE ';'.
* Level 3: split hierarchy infoobjects
CONSTANTS c_sep_lvl3 TYPE c VALUE '@'.
CONSTANTS c_aggregated TYPE string VALUE 'ALL'.

TYPES:
      BEGIN OF tp_iobj_cmp_size,

        iobj TYPE rsiobjnm,
        size TYPE i,

      END OF tp_iobj_cmp_size.

DATA lv_relat_per TYPE i.
DATA lt_iobj_cmp_size TYPE TABLE OF tp_iobj_cmp_size.
DATA lt_qip_definitions TYPE TABLE OF zqip_definition.

DATA lv_fiscper3 TYPE /bi0/oifiscper3.
DATA lv_fiscper TYPE /bi0/oifiscper.
DATA lv_fiscyear TYPE /bi0/oifiscyear.

DATA lv_calyear TYPE /bi0/oicalyear.
DATA lv_calmonth TYPE /bi0/oicalmonth.
DATA lv_calmonth2 TYPE /bi0/oicalmonth2.

DATA lv_curtype TYPE /bi0/oicurtype.
DATA lv_qty_records TYPE i.

FIELD-SYMBOLS <ls_qip_definition> TYPE zqip_definition.

DATA lv_fiscvarnt TYPE /bi0/oifiscvarnt.
DATA lv_cs_dimen  TYPE /bi0/oics_dimen.

DATA lt_period TYPE TABLE OF /bi0/oifiscper.
DATA ls_period TYPE /bi0/oifiscper.
*functions parameters
DATA lt_parameters TYPE rrxw3tquery.
DATA lt_axis_info TYPE rrws_thx_axis_info.
DATA lt_cells TYPE rrws_t_cell.
DATA lt_axis_data TYPE rrws_thx_axis_data.
DATA lt_texts TYPE rrws_t_text_symbols.

DATA lt_target_table TYPE REF TO data.
DATA lt_iobj TYPE TABLE OF rsd_s_viobj.
DATA lt_comp TYPE rsdo_t_cmp.
DATA gv_dec_sep TYPE c.

DATA lt_object_lock TYPE TABLE OF eqegraarg.

DATA lv_default_kf TYPE rsiobjnm.

DATA gt_nodes_a_leaves TYPE rshi_t_hienode.
DATA gt_interval TYPE rshi_th_interval.
