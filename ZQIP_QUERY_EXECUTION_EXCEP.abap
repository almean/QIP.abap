*&---------------------------------------------------------------------*
*&  Include           ZQIP_QUERY_EXECUTION_EXCEP
*&---------------------------------------------------------------------*
FORM process_exception USING p_iobj_from TYPE rsiobjnm
                             p_value_from TYPE rsatrvl
                       CHANGING p_iobj_to TYPE rsiobjnm
                                p_value_to TYPE rsatrvl.

  CLEAR p_iobj_to.
  CLEAR p_value_to.
  CASE p_iobj_from.

    WHEN '0COUNTRY'.

      p_iobj_to = 'HREP_ENT'.
      CASE p_value_from.

        WHEN 'AR'.

          p_value_to = '01G_MINE'.

        WHEN 'BR'.

          p_value_to = '01G_CIM'.

        WHEN 'CL'.

          p_value_to = '01G_CP'.

        WHEN 'CR'.

          p_value_to = '01G_INC'.

        WHEN 'MX'.

          p_value_to = '01G_MEX'.

        WHEN 'CO'.

          p_value_to = '01G_CB'.

        WHEN 'EC'.

          p_value_to = '01G_LCN'.

        WHEN 'SV'.

          p_value_to = '01G_SLV'.

        WHEN 'NI'.

          p_value_to = '01G_NICA'.

        WHEN OTHERS.

      ENDCASE.

    WHEN 'ZSUB_SEG'.

      p_iobj_to = 'HSUBSEGM'.
      CASE p_value_from.

        WHEN '01'. "Clinker & Cement

          p_value_to = '1000'.

        WHEN '05'. "Aggregates

          p_value_to = '2000'.

        WHEN '06'. "Concrete

          p_value_to = '3300'.

        WHEN '07'. "Concrete Products

          p_value_to = '3400'.

      ENDCASE.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
