CREATE OR REPLACE PACKAGE BODY SINIESTROS.PKG_REMANENTE IS

  TYPE t_poliza IS TABLE OF mpolizas%ROWTYPE INDEX BY PLS_INTEGER;

  aPoliza t_poliza;

  TYPE t_situacion IS TABLE OF mpolisit%ROWTYPE INDEX BY PLS_INTEGER;

  aSituacion t_Situacion;

  TYPE t_100_30 IS TABLE OF VARCHAR2(100) INDEX BY VARCHAR2(30);

  aTvalosit t_100_30;

  aTvalogar t_100_30;

  aTvalogarAux t_100_30;

  TYPE T_SINIESTRO IS TABLE OF MSINIEST%ROWTYPE INDEX BY PLS_INTEGER;

  aSiniestro t_siniestro;

  k_1 CONSTANT PLS_INTEGER := 1;

  k_s CONSTANT VARCHAR2(1) := 'S';

  TYPE r_FecCob IS RECORD(
     vCdGarant      VARCHAR2(4)
    ,nSumaAsegurada NUMBER
    ,vMoneda        VARCHAR2(4));

  TYPE T_COBERTURA IS TABLE OF r_FecCob INDEX BY PLS_INTEGER;

  aCobertura T_COBERTURA;

  aCoberturaAux T_COBERTURA;

  TYPE r_Negocio IS RECORD(
     CdAnio    negocio.MNEGOCIO.CDANIO%TYPE
    ,NMNEGOCIO negocio.MNEGOCIO.NMNEGOCIO%TYPE
    ,Nmempresa negocio.mnegoemp.nmempresa%TYPE
    ,NmPoliza  negocio.mnegpoliza.nmpoliza%TYPE
    ,CdFilial  negocio.mnegpolfil.cdfilial%TYPE
    ,CdGrupo   negocio.mnegpolgru.cdgrupo%TYPE);

  TYPE T_NEGOCIO IS TABLE OF r_Negocio INDEX BY PLS_INTEGER;

  aNegocio T_NEGOCIO;

  n_nTramite TMESACONTROL.NTRAMITE%TYPE;

  TYPE T_ICD IS TABLE OF MSINIICD%ROWTYPE INDEX BY PLS_INTEGER;

  aIcd T_ICD;

  TYPE t_mesacontrol IS TABLE OF tmesacontrol%ROWTYPE INDEX BY PLS_INTEGER;

  aMesaControl t_mesacontrol;

  TYPE T_VARIABLES IS TABLE OF VARCHAR2(100) INDEX BY VARCHAR2(30);

  aVariables T_VARIABLES;

  TYPE t_vtmesacontrol IS TABLE OF VARCHAR2(100) INDEX BY VARCHAR2(255);

  aVTMesaControl t_vtmesacontrol;

  TYPE t_otvalor IS TABLE OF VARCHAR2(100) INDEX BY VARCHAR2(30);

  TYPE t_remanente IS TABLE OF t_otvalor INDEX BY PLS_INTEGER;

  aRemanente t_remanente;

  TYPE T_CDCONCEP IS TABLE OF t_otvalor INDEX BY VARCHAR2(3);

  TYPE T_CDTIPVAL IS TABLE OF T_CDCONCEP INDEX BY VARCHAR2(3);

  TYPE T_CDGARANT IS TABLE OF T_CDTIPVAL INDEX BY VARCHAR2(4);

  aTvaloVal T_CDGARANT;

  -- --
  v_CdTipo TSINIGAR.CDTIPO%TYPE;

  TYPE r_rema_cont IS RECORD(
     IdRemanente PLS_INTEGER
    ,CdNivel     VARCHAR2(2)
    ,cdconval    VARCHAR2(20));

  TYPE t_rema_cont IS TABLE OF r_rema_cont INDEX BY PLS_INTEGER;

  aRemaCont t_rema_cont;

  -- --
  b_VariablesPlanchadas BOOLEAN := FALSE;

  PROCEDURE LOAD_DATOS(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
    v_CdGarant tgaranti.cdgarant%TYPE;
    PROCEDURE LOAD_MESA_CONTROL(pn_Ntramite_i IN PLS_INTEGER) IS
    BEGIN
      SELECT *
        BULK COLLECT
        INTO aMesaControl
        FROM ice.tmesacontrol t
       WHERE t.ntramite = pn_Ntramite_i;
    END;
  
    PROCEDURE LOAD_SINIESTRO(pn_cdunieco_i IN msiniicd.cdunieco%TYPE
                            ,pn_cdramo_i   IN msiniicd.cdramo%TYPE
                            ,pn_aaapertu_i IN msiniicd.aaapertu%TYPE
                            ,pn_nmsinies_i IN msiniicd.nmsinies%TYPE
                            ,pn_ntramite_i IN ice.tmesacontrol.ntramite %TYPE) IS
      CURSOR cur_siniestro IS
        SELECT *
          FROM msiniest ms
         WHERE ms.cdunieco = pn_cdunieco_i
           AND ms.cdramo = pn_cdramo_i
           AND ms.aaapertu = pn_aaapertu_i
           AND ms.nmsinies = pn_nmsinies_i;
      CURSOR cur_icd IS
        SELECT m.*
          FROM msiniicd m
         WHERE m.cdunieco = pn_cdunieco_i
           AND m.cdramo = pn_cdramo_i
           AND m.aaapertu = pn_aaapertu_i
           AND m.nmsinies = pn_nmsinies_i
           AND (m.ntramite = pn_ntramite_i OR pn_ntramite_i IS NULL)
           AND m.swprinci = k_s;
    BEGIN
      OPEN Cur_Siniestro;
      FETCH Cur_Siniestro BULK COLLECT
        INTO aSiniestro LIMIT 120;
      CLOSE cur_Siniestro;
      OPEN Cur_ICD;
      FETCH Cur_ICD BULK COLLECT
        INTO aIcd LIMIT 120;
      CLOSE Cur_ICD;
    END;
  
    PROCEDURE LOAD_POLIZA IS
      CURSOR Cur_poliza IS
        SELECT *
          FROM MPOLIZAS MPO
         WHERE MPO.CDUNIECO = aSiniestro(1).Cdunieco
           AND mpo.cdramo = aSiniestro(1).CdRamo
           AND mpo.estado = 'M'
           AND mpo.nmpoliza = aSiniestro(1).NmPoliza
           AND mpo.nmsuplem =
               (SELECT MAX(NMSUPLEM)
                  FROM MPOLIZAS MPO2
                 WHERE MPO2.CDUNIECO = mpo.cdunieco
                   AND mpo2.cdramo = mpo.cdramo
                   AND mpo2.estado = mpo.estado
                   AND mpo2.nmpoliza = mpo.nmpoliza
                   AND mpo2.nmsuplem <= aSiniestro(1).Nmsuplem);
    BEGIN
      aPoliza.Delete();
      OPEN cur_poliza;
      FETCH cur_poliza BULK COLLECT
        INTO aPoliza LIMIT 120;
      CLOSE cur_poliza;
    END;
  
    PROCEDURE LOAD_TVALOVAL IS
      CURSOR Cur_v IS
        SELECT 'OTVALOR' || CASE
                 WHEN V.CDATRIBU < 100 THEN
                  TRIM(TO_CHAR(V.CDATRIBU
                              ,'00'))
                 ELSE
                  TRIM(TO_CHAR(V.CDATRIBU))
               END AS CDATRIBU
              ,V.OTVALOR
              ,V.CDGARANT
              ,V.CDTIPVAL
              ,V.CDCONCEP
          FROM VTVALOVAL V
          JOIN TSINIGAR T
            ON T.CDGARANT = V.CDGARANT
           AND T.NTRAMITE = V.NTRAMITE
         WHERE T.NTRAMITE = aMesaControl(1).Ntramite
           AND IDATRIBU IN ('UNIDADREMANENTE'
                           ,'UNIDAD'
                           ,'MONEDAIMPORTEFACTURA'
                           ,'IMPORTEFACTURA'
                           ,'IMPORTE'
                           ,'CANTUNIDAPROBADAS'
                           ,'CANTIDAD'
                           ,'I'
                           ,'DINDEMREC')
         ORDER BY V.CDGARANT
                 ,V.CDCONVAL
                 ,V.CDTIPVAL
                 ,V.CDCONCEP;
      Reg_V Cur_V%ROWTYPE;
    BEGIN
      OPEN Cur_V;
      LOOP
        FETCH Cur_V
          INTO Reg_V;
        EXIT WHEN Cur_V%NOTFOUND;
        aTvaloVal(Reg_V.CDGARANT)(rEG_v.CDTIPVAL)(rEG_v.CDCONCEP)(rEG_v.CDATRIBU) := Reg_V.OTValor;
      END LOOP;
      CLOSE Cur_V;
    END;
  
    PROCEDURE LOAD_TVALOSIT IS
      CURSOR Cur_v IS
        SELECT 'OTVALOR' || CASE
                 WHEN V.CDATRIBU < 100 THEN
                  TRIM(TO_CHAR(CDATRIBU
                              ,'00'))
                 ELSE
                  TRIM(TO_CHAR(V.CDATRIBU))
               END AS CDATRIBU
              ,OTVALOR
          FROM VTVALOSIT V
         WHERE V.CDUNIECO = aSituacion(1).cdunieco
           AND V.cdramo = aSituacion(1).cdramo
           AND V.estado = aSituacion(1).ESTADO
           AND V.nmpoliza = aSituacion(1).nmpoliza
           AND V.NMSITUAC = aSituacion(1).nmsituac
           AND V.nmsuplem =
               (SELECT MAX(NMSUPLEM)
                  FROM TVALOSIT MPS2
                 WHERE MPS2.CDUNIECO = V.cdunieco
                   AND MPS2.cdramo = V.cdramo
                   AND MPS2.estado = V.estado
                   AND MPS2.nmpoliza = V.nmpoliza
                   AND MPS2.NMSITUAC = V.NMSITUAC
                   AND MPS2.nmsuplem <= aSiniestro(1).Nmsuplem);
      Reg_V Cur_V%ROWTYPE;
    BEGIN
      OPEN Cur_V;
      LOOP
        FETCH Cur_V
          INTO Reg_V;
        EXIT WHEN Cur_V%NOTFOUND;
        aTvalosit(Reg_V.CdAtribu) := Reg_V.OTValor;
      END LOOP;
      CLOSE Cur_V;
    END;
  
    PROCEDURE LOAD_SITUACION IS
      CURSOR Cur_Situacion IS
        SELECT *
          FROM MPOLISIT MPS
         WHERE MPS.CDUNIECO = aPoliza(1).cdunieco
           AND mps.cdramo = aPoliza(1).cdramo
           AND mps.estado = aPoliza(1).ESTADO
           AND mps.nmpoliza = aPoliza(1).nmpoliza
           AND mps.NMSITUAC = aSiniestro(1).nmsituac
           AND mps.nmsuplem =
               (SELECT MAX(NMSUPLEM)
                  FROM MPOLISIT MPS2
                 WHERE MPS2.CDUNIECO = MPS.cdunieco
                   AND MPS2.cdramo = MPS.cdramo
                   AND MPS2.estado = MPS.estado
                   AND MPS2.nmpoliza = MPS.nmpoliza
                   AND MPS2.NMSITUAC = MPS.NMSITUAC
                   AND MPS2.nmsuplem <= aSiniestro(1).Nmsuplem);
    BEGIN
      aSituacion.Delete();
      OPEN Cur_Situacion;
      FETCH cur_situacion BULK COLLECT
        INTO aSituacion LIMIT 120;
      CLOSE cur_Situacion;
    END;
  
    PROCEDURE LOAD_COBERTURA(pv_CdGarant_i IN MPOLIGAR.CDGARANT%TYPE
                            ,pv_Aux_i      IN VARCHAR2 DEFAULT 'N') IS
      CURSOR Cur_Cobertura IS
        SELECT MPG.CDGARANT
              ,mpc.ptcapita
              ,VTG.otvalor
          FROM ice.MPOLIGAR mpg
          JOIN MPOLICAP mpc
            ON mpc.cdunieco = mpg.cdunieco
           AND mpc.cdramo = mpg.cdramo
           AND mpc.estado = mpg.estado
           AND mpc.nmpoliza = mpg.nmpoliza
           AND mpc.nmsituac = mpg.nmsituac
           AND mpc.cdcapita = mpg.cdcapita
           AND MPC.NMSUPLEM =
               (SELECT MAX(NMSUPLEM)
                  FROM MPOLICAP mpc2
                 WHERE mpc2.cdunieco = mpC.cdunieco
                   AND mpc2.cdramo = mpC.cdramo
                   AND mpc2.estado = mpC.estado
                   AND mpc2.nmpoliza = mpC.nmpoliza
                   AND mpc2.nmsituac = mpC.nmsituac
                   AND mpc2.cdcapita = mpC.cdcapita
                   AND MPC2.NMSUPLEM <= aSiniestro(1).NmSuplem)
          JOIN vtvalogar vtg
            ON vtg.cdunieco = mpg.cdunieco
           AND vtg.cdramo = mpg.cdramo
           AND vtg.estado = mpg.estado
           AND vtg.nmpoliza = mpg.nmpoliza
           AND vtg.nmsituac = mpg.nmsituac
           AND vtg.cdgarant = mpg.cdgarant
           AND vtg.idatribu IN ('MONEDASA'
                               ,'MONEDADESUBLIMITE')
           AND VTG.nmsuplem =
               (SELECT MAX(NMSUPLEM)
                  FROM tvalogar vtg2
                 WHERE vtg2.cdunieco = vtg.cdunieco
                   AND vtg2.cdramo = vtg.cdramo
                   AND vtg2.estado = vtg.estado
                   AND vtg2.nmpoliza = vtg.nmpoliza
                   AND vtg2.nmsituac = vtg.nmsituac
                   AND vtg2.cdgarant = vtg.cdgarant
                   AND VTG2.NMSUPLEM <= aSiniestro(1).NmSuplem)
         WHERE mpg.cdunieco = aSituacion(1).CdUnieco
           AND mpg.cdramo = aSituacion(1).CdRamo
           AND mpg.estado = aSituacion(1).Estado
           AND mpg.nmpoliza = aSituacion(1).NmPoliza
           AND mpg.nmsituac = aSituacion(1).NmSituac
           AND mpg.cdgarant = pv_CdGarant_i
           AND mpg.nmsuplem =
               (SELECT MAX(nmsuplem)
                  FROM ice.MPOLIGAR MPG2
                 WHERE MPG2.cdunieco = mpg.cdunieco
                   AND MPG2.cdramo = mpg.cdramo
                   AND MPG2.estado = mpg.estado
                   AND MPG2.nmpoliza = mpg.nmpoliza
                   AND MPG2.NMSITUAC = MPG.NMSITUAC
                   AND MPG2.CDGARANT = MPG.CDGARANT
                   AND MPG2.nmsuplem <= aSiniestro(1).NmSuplem);
      Reg_Cobertura Cur_Cobertura%ROWTYPE;
      v_Dummy       VARCHAR2(300);
    BEGIN
      OPEN Cur_Cobertura;
      LOOP
        FETCH Cur_Cobertura
          INTO Reg_Cobertura;
        EXIT WHEN Cur_Cobertura%NOTFOUND;
        IF pv_Aux_i = 'N' THEN
          aCobertura(1).vCdGarant := Reg_Cobertura.CdGarant;
          aCobertura(1).nSumaAsegurada := Reg_Cobertura.Ptcapita;
          aCobertura(1).vMoneda := Reg_Cobertura.Otvalor;
        ELSE
          aCoberturaAux(1).vCdGarant := Reg_Cobertura.CdGarant;
          aCoberturaAux(1).nSumaAsegurada := Reg_Cobertura.Ptcapita;
          aCoberturaAux(1).vMoneda := Reg_Cobertura.Otvalor;
        END IF;
      END LOOP;
      CLOSE cur_Cobertura;
      IF pv_Aux_i = 'N' THEN
        BEGIN
          v_Dummy := aCobertura(1).vCdGarant;
        EXCEPTION
          WHEN no_data_found THEN
            v_Dummy := 'No se encontró información de la cobertura para la fecha del siniestro';
            pkg_traduc.p_inserta_bitacora(pi_msg_id   => 10000
                                         ,pi_code_err => SQLCODE
                                         ,pi_msg_text => v_Dummy
                                         ,pi_usuario  => USER
                                         ,pi_programa => 'PKG_REMANENTE.LOAD_COBERTURA'
                                         ,pi_dsaccion => 'Error '
                                         ,pi_tipo     => 1);
            raise_application_Error(-20001
                                   ,v_Dummy);
        END;
      ELSE
        BEGIN
          v_Dummy := aCoberturaAux(1).vCdGarant;
        EXCEPTION
          WHEN no_data_found THEN
            v_Dummy := 'No se encontró información de la cobertura auxiliar para la fecha del siniestro';
            pkg_traduc.p_inserta_bitacora(pi_msg_id   => 10000
                                         ,pi_code_err => SQLCODE
                                         ,pi_msg_text => v_Dummy
                                         ,pi_usuario  => USER
                                         ,pi_programa => 'PKG_REMANENTE.LOAD_COBERTURA'
                                         ,pi_dsaccion => 'Error '
                                         ,pi_tipo     => 1);
            raise_application_Error(-20001
                                   ,v_Dummy);
        END;
      END IF;
    END;
  
    PROCEDURE LOAD_TVALOGAR(pv_CdGarant_i IN VARCHAR2
                           ,pv_Aux_i      IN VARCHAR2 DEFAULT 'N') IS
      CURSOR Cur_v IS
        SELECT 'OTVALOR' || CASE
                 WHEN V.CDATRIBU < 100 THEN
                  TRIM(TO_CHAR(CDATRIBU
                              ,'00'))
                 ELSE
                  TRIM(TO_CHAR(V.CDATRIBU))
               END AS CDATRIBU
              ,OTVALOR
          FROM VTVALOGAR V
         WHERE V.CDUNIECO = aSituacion(1).cdunieco
           AND V.cdramo = aSituacion(1).cdramo
           AND V.estado = aSituacion(1).ESTADO
           AND V.nmpoliza = aSituacion(1).nmpoliza
           AND V.NMSITUAC = aSituacion(1).nmsituac
           AND V.cdgarant = pv_CdGarant_i
           AND V.nmsuplem =
               (SELECT MAX(NMSUPLEM)
                  FROM tvalogar MPS2
                 WHERE MPS2.CDUNIECO = V.cdunieco
                   AND MPS2.cdramo = V.cdramo
                   AND MPS2.estado = V.estado
                   AND MPS2.nmpoliza = V.nmpoliza
                   AND MPS2.NMSITUAC = V.NMSITUAC
                   AND mps2.cdgarant = v.cdgarant
                   AND MPS2.nmsuplem <= aSiniestro(1).Nmsuplem);
      Reg_V Cur_V%ROWTYPE;
    BEGIN
      OPEN Cur_V;
      LOOP
        FETCH Cur_V
          INTO Reg_V;
        EXIT WHEN Cur_V%NOTFOUND;
        IF pv_Aux_i = 'N' THEN
          aTvalogar(Reg_V.CdAtribu) := Reg_V.OTValor;
        ELSE
          aTvalogarAux(Reg_V.CdAtribu) := Reg_V.OTValor;
        END IF;
      END LOOP;
      CLOSE Cur_V;
    END;
  
    PROCEDURE LOAD_CLAVE_NEGOCIO IS
      CURSOR Cur_Negocio IS
        SELECT OTVALOR03 ANIO
              ,OTVALOR04 NEGOCIO
              ,OTVALOR05 EMPRESA
              ,OTVALOR06 POLIZA
          FROM TMESACONTROL TM
         WHERE CDTIPFLU = 1
           AND CDFLUJOMC = 100
           AND TM.CDUNIECO = aPoliza(1).CdUnieco
           AND TM.CDRAMO = aPoliza(1).CdRamo
           AND TM.ESTADO = aPoliza(1).estado
           AND TM.NMPOLIZA = aPoliza(1).nmPoliza;
      Reg_Negocio Cur_Negocio%ROWTYPE;
      CURSOR Cur_Grupo IS
        SELECT cdFilial
          FROM MPOLIGRU MPG
         WHERE mpg.CDUNIECO = aPoliza(1).CdUnieco
           AND mpg.CDRAMO = aPoliza(1).CdRamo
           AND mpg.ESTADO = aPoliza(1).estado
           AND mpg.NMPOLIZA = aPoliza(1).nmPoliza
           AND mpg.cdgrupo = aSituacion(1).CdGrupo
           AND mpg.nmsuplem =
               (SELECT MAX(nmsuplem)
                  FROM MPOLIGRU mpg2
                 WHERE mpg2.CDUNIECO = mpg.CdUnieco
                   AND mpg2.CDRAMO = mpg.CdRamo
                   AND mpg2.ESTADO = mpg.estado
                   AND mpg2.NMPOLIZA = mpg.nmPoliza
                   AND mpg2.cdgrupo = mpg.CdGrupo
                   AND mpg2.nmsuplem <= aSiniestro(1).NmSuplem);
      Reg_Grupo Cur_Grupo%ROWTYPE;
    BEGIN
      OPEN Cur_Negocio;
      FETCH Cur_Negocio
        INTO Reg_Negocio;
      CLOSE cur_Negocio;
      aNegocio(1).CdAnio := Reg_Negocio.Anio;
      aNegocio(1).NMNEGOCIO := Reg_Negocio.Negocio;
      aNegocio(1).Nmempresa := Reg_Negocio.Empresa;
      aNegocio(1).NmPoliza := Reg_Negocio.Poliza;
      aNegocio(1).CdGrupo := aSituacion(1).CdGrupo;
      OPEN Cur_Grupo;
      FETCH Cur_Grupo
        INTO Reg_Grupo;
      CLOSE Cur_Grupo;
      aNegocio(1).CdFilial := Reg_Grupo.Cdfilial;
    END;
  
    PROCEDURE LOAD_VTMESACONTROL(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
      CURSOR Cur_V IS
        SELECT upper(DSATRIBU) AS dsatribu
              ,OTVALOR
          FROM vtmesacontrol v
         WHERE v.ntramite = pn_ntramite_i;
      Reg_V Cur_V%ROWTYPE;
    BEGIN
      OPEN Cur_V;
      LOOP
        FETCH cur_v
          INTO reg_v;
        EXIT WHEN cur_v%NOTFOUND;
        aVTMesaControl(Reg_V.Dsatribu) := reg_v.otvalor;
      END LOOP;
      CLOSE cur_v;
    END;
  
    FUNCTION F_COBERTURA_GET(pn_NTramite_i IN TMESACONTROL.NTRAMITE%TYPE
                            ,pv_CdTipo_i   IN VARCHAR2) RETURN VARCHAR2 IS
      v_Return VARCHAR2(4);
    BEGIN
      BEGIN
        SELECT t.cdgarant
          INTO v_return
          FROM tsinigar t
         WHERE t.ntramite = pn_Ntramite_i
           AND T.CDTIPO = pv_CdTipo_i;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          V_RETURN := NULL;
      END;
      RETURN v_Return;
    END;
  
    PROCEDURE LOAD_VARIABLES IS
    BEGIN
      aVariables('ESTADO') := aPoliza(1).estado;
      aVariables('NMPOLIZA') := aPoliza(1).nmpoliza;
      aVariables('NMSUPLEM') := aSiniestro(1).nmsuplem;
      aVariables('NMSITUAC') := aSituacion(1).NMSITUAC;
      aVariables('NMNEGOCIO') := aNegocio(1).NmNegocio;
      aVariables('CDANIO') := aNegocio(1).CDANIO;
      aVariables('NMEMPRESA') := aNegocio(1).NMEMPRESA;
      aVariables('NMNEGPOL') := aNegocio(1).NmPoliza;
      aVariables('CDFILIAL') := aNegocio(1).CDFILIAL;
      aVariables('CDGRUPO') := aNegocio(1).CDGRUPO;
      aVariables('STATUS') := aSiniestro(1).STATUS;
      IF NOT b_VariablesPlanchadas THEN
        aVariables('CDUNIECO') := aPoliza(1).CdUnieco;
        aVariables('CDRAMO') := aPoliza(1).cdramo;
        IF NVL(v_CdTipo
              ,'P') = 'P' THEN
          aVariables('CDGARANT') := aCobertura(1).vCdGarant;
        ELSE
          aVariables('CDGARANT') := aCoberturaAux(1).vCdGarant;
        END IF;
        aVariables('CDICD') := aIcd(1).CDCOICD;
        aVariables('AAAPERTU') := aSiniestro(1).Aaapertu;
        aVariables('NMSINIES') := aSiniestro(1).NMSINIES;
      END IF;
    END;
  
  BEGIN
    n_nTramite := pn_ntramite_i;
    --
    IF n_nTramite IS NOT NULL THEN
      load_mesa_control(n_nTramite);
      LOAD_VTMESACONTROL(n_nTramite);
      LOAD_TVALOVAL;
    END IF;
    --
    IF b_VariablesPlanchadas THEN
      load_siniestro(aVariables('CDUNIECO')
                    ,aVariables('CDRAMO')
                    ,aVariables('AAAPERTU')
                    ,aVariables('NMSINIES')
                    ,n_nTramite);
    ELSE
      load_siniestro(aMesaControl(1).cdunieco
                    ,aMesaControl(1).cdramo
                    ,aMesaControl(1).otvalor24
                    ,aMesaControl(1).otvalor25
                    ,n_nTramite);
    END IF;
    load_poliza;
    load_situacion;
    LOAD_TVALOSIT;
    --
    --Cobertura de la base del dictamen
    IF b_VariablesPlanchadas THEN
      v_CdGarant := aVariables('CDGARANT');
    ELSE
      v_CdGarant := F_COBERTURA_GET(pn_ntramite_i
                                   ,'P');
    END IF;
    LOAD_COBERTURA(v_CdGarant);
    LOAD_TVALOGAR(v_CdGarant);
    --Cobertura tipo Complemento o Preexistencia
    IF NOT b_VariablesPlanchadas THEN
      v_CdGarant := F_COBERTURA_GET(pn_ntramite_i
                                   ,'C');
    ELSE
      v_CdGarant := NULL;
    END IF;
    IF v_CdGarant IS NOT NULL THEN
      LOAD_COBERTURA(v_CdGarant
                    ,k_s);
      LOAD_TVALOGAR(v_CdGarant
                   ,k_s);
    ELSE
      --Cobertura tipo Territorialidad
      IF NOT b_VariablesPlanchadas THEN
        v_CdGarant := F_COBERTURA_GET(pn_ntramite_i
                                     ,'T');
      ELSE
        v_CdGarant := NULL;
      END IF;
      IF v_CdGarant IS NOT NULL THEN
        LOAD_COBERTURA(v_CdGarant
                      ,k_s);
        LOAD_TVALOGAR(v_CdGarant
                     ,k_s);
      END IF;
    END IF;
    --
    LOAD_CLAVE_NEGOCIO;
    LOAD_VARIABLES;
  END;

  FUNCTION F_TVALOREMA_GET(pv_CdNivel_i     IN VARCHAR2
                          ,pn_CdRemanente_i IN PLS_INTEGER
                          ,pv_CdTipoGar_i   IN VARCHAR2 DEFAULT 'P')
    RETURN PLS_INTEGER IS
    v_SQL VARCHAR2(4000);
    CURSOR Cur_T(pv_TpField_i VARCHAR2) IS
      SELECT t.idatribu
            ,t.cdatribu
        FROM tatrirema t
       WHERE CDNIVEL = pv_CdNivel_i
         AND TPFIELD = pv_TpField_i;
    reg_t    Cur_T%ROWTYPE;
    i_Return PLS_INTEGER;
  BEGIN
    v_SQL := 'SELECT IDREMANENTE,OTVALOR01,OTVALOR02,OTVALOR03,OTVALOR04,OTVALOR05 ';
    v_sql := v_sql || ' FROM TVALOREMA ';
    v_sql := v_sql || ' WHERE cdnivel = :1 and cdremanente = :2 ';
    OPEN cur_t('C');
    LOOP
      FETCH cur_t
        INTO reg_t;
      EXIT WHEN cur_t%NOTFOUND;
      IF reg_t.Idatribu = 'CDGARANT' AND pv_CdTipoGar_i != 'P' THEN
        v_sql := v_sql || ' AND OTCLAVE' || Reg_T.cdatribu || '=' ||
                 chr(39) || aCoberturaAux(1).vCdGarant || chr(39);
      ELSE
        v_sql := v_sql || ' AND OTCLAVE' || Reg_T.cdatribu || '=' ||
                 chr(39) || aVariables(Reg_T.idatribu) || chr(39);
      END IF;
    END LOOP;
    CLOSE cur_T;
    dbms_output.put_line(v_sql);
    BEGIN
      EXECUTE IMMEDIATE v_sql
        INTO i_Return, aRemanente(pn_CdRemanente_i)('OTVALOR01'), aRemanente(pn_CdRemanente_i)('OTVALOR02'), aRemanente(pn_CdRemanente_i)('OTVALOR03'), aRemanente(pn_CdRemanente_i)('OTVALOR04'), aRemanente(pn_CdRemanente_i)('OTVALOR05')
        USING PV_CDNIVEL_I, pn_CdRemanente_i;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        i_Return := NULL;
    END;
    RETURN i_Return;
  END;

  FUNCTION F_TVALOREMA_INS(pv_CdNivel_i     IN VARCHAR2
                          ,pn_CdRemanente_i IN PLS_INTEGER
                          ,pv_CdTipoGar_i   IN VARCHAR2 DEFAULT 'P')
    RETURN PLS_INTEGER IS
    v_SQL VARCHAR2(4000);
    CURSOR Cur_T(pv_TpField_i VARCHAR2) IS
      SELECT t.idatribu
            ,t.cdatribu
        FROM tatrirema t
       WHERE CDNIVEL = pv_CdNivel_i
         AND TPFIELD = pv_TpField_i;
    reg_t         Cur_T%ROWTYPE;
    n_IdRemanente TVALOREMA.IDREMANENTE%TYPE;
  BEGIN
    v_SQL := 'INSERT INTO TVALOREMA (cdnivel,cdremanente,';
    OPEN cur_t('C');
    LOOP
      FETCH cur_t
        INTO reg_t;
      EXIT WHEN cur_t%NOTFOUND;
      v_sql := v_sql || 'OTCLAVE' || Reg_T.cdatribu || ',';
    END LOOP;
    CLOSE cur_T;
    v_sql := v_sql || '*';
    v_sql := REPLACE(v_sql
                    ,',*');
    v_sql := v_sql || ') values (:1,:2,';
    OPEN cur_t('C');
    LOOP
      FETCH cur_t
        INTO reg_t;
      EXIT WHEN cur_t%NOTFOUND;
      IF reg_t.Idatribu = 'CDGARANT' AND pv_CdTipoGar_i != 'P' THEN
        v_sql := v_sql || CHR(39) || aCoberturaAux(1).vCdGarant || CHR(39) || ',';
      ELSE
        v_sql := v_sql || CHR(39) || aVariables(Reg_T.idatribu) || CHR(39) || ',';
      END IF;
    END LOOP;
    CLOSE cur_T;
    v_sql := v_sql || '*';
    v_sql := REPLACE(v_sql
                    ,',*');
    v_sql := v_sql || ') returning idremanente into :3';
    --
    --    dbms_output.put_line(v_sql);
    EXECUTE IMMEDIATE v_sql
      USING IN pv_CdNivel_i, IN pn_CdRemanente_i, OUT n_IdRemanente;
    RETURN n_IdRemanente;
  END;

  PROCEDURE P_TVALOREMA_UPD(pn_idremanente_i IN tvalorema.idremanente%TYPE
                           ,pv_otvalor01_i   IN tvalorema.otvalor01%TYPE
                           ,pv_otvalor02_i   IN tvalorema.otvalor02%TYPE DEFAULT NULL) IS
  BEGIN
    UPDATE TVALOREMA
       SET OTVALOR01 = pv_Otvalor01_i
          ,otvalor02 = NVL(pv_otvalor02_i
                          ,otvalor02)
     WHERE idremanente = pn_idremanente_i;
  END;

  FUNCTION F_SUM_OTVALOR01_GET(pn_idremanente_i IN tvalorema.idremanente%TYPE
                              ,pv_status_i      IN VARCHAR2) RETURN NUMBER IS
    n_Return NUMBER;
  BEGIN
    BEGIN
      SELECT SUM(otvalor01)
        INTO n_Return
        FROM tvaloremadet d
       WHERE d.idremanente = pn_IdRemanente_i
         AND d.status = pv_status_i;
    END;
    RETURN NVL(n_Return
              ,0);
  END;

  FUNCTION F_MAX_NUMORD(pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                       ,pv_Status_i      IN VARCHAR2 DEFAULT NULL)
    RETURN PLS_INTEGER IS
    i_Return PLS_INTEGER;
  BEGIN
    BEGIN
      SELECT nvl(MAX(T.NMORDINA)
                ,0)
        INTO i_Return
        FROM TVALOREMADET T
       WHERE t.idremanente = pn_IdRemanente_i
         AND (T.STATUS = pv_Status_i OR pv_Status_i IS NULL);
    EXCEPTION
      WHEN OTHERS THEN
        i_Return := 0;
    END;
    RETURN i_Return;
  END;

  FUNCTION F_TVALOREMADET_EXISTE_GET(pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                                    ,pn_NmOrdina_i    IN TVALOREMADET.nmordina%TYPE
                                    ,pn_ntramite_i    IN TVALOREMADET.NTRAMITE%TYPE
                                    ,pv_CdConval_i    IN TVALOREMADET.cdconval%TYPE
                                    ,pv_Status_i      IN TVALOREMADET.STATUS%TYPE)
    RETURN BOOLEAN IS
    CURSOR Cur_R IS
      SELECT rownum
        FROM tvaloremadet t
       WHERE t.idremanente = pn_IdRemanente_i
         AND t.nmordina = pn_nmordina_i
         AND t.cdconval = pv_CdConval_i
         AND t.ntramite = pn_ntramite_i
         AND t.status = pv_Status_i;
    i_Dummy  PLS_INTEGER;
    b_Return BOOLEAN := FALSE;
  BEGIN
    OPEN cur_r;
    FETCH cur_R
      INTO i_Dummy;
    b_Return := Cur_R%FOUND;
    CLOSE cur_r;
    RETURN b_Return;
  END;

  PROCEDURE P_TVALOREMADET_INS(pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                              ,pn_NmOrdina_i    IN TVALOREMADET.nmordina%TYPE                            
                              ,pv_CdConval_i    IN TVALOREMADET.cdconval%TYPE
                              ,pv_Status_i      IN TVALOREMADET.STATUS%TYPE
                              ,pd_FESTATUS_i    IN TVALOREMADET.FESTATUS%TYPE
                              ,pv_OtValor01_i   IN TVALOREMADET.otvalor01%TYPE
                              ,pv_OtValor02_i   IN TVALOREMADET.otvalor02%TYPE
                              ,pv_OtValor03_i   IN TVALOREMADET.otvalor03%TYPE DEFAULT NULL
                              ,pv_OtValor04_i   IN TVALOREMADET.otvalor04%TYPE DEFAULT NULL
                              ,pv_OtValor05_i   IN TVALOREMADET.otvalor05%TYPE DEFAULT NULL) IS
    n_NmOrdina PLS_INTEGER;
  BEGIN
    n_NmOrdina := pn_NmOrdina_i;
    IF n_NmOrdina IS NULL THEN
      n_NmOrdina := F_MAX_NUMORD(pn_IdRemanente_i) + 1;
    END IF;
    INSERT INTO tvaloremadet
      (idremanente
      ,nmordina
      ,ntramite
      ,cdconval
      ,status
      ,festatus
      ,otvalor01
      ,otvalor02
      ,otvalor03
      ,otvalor04
      ,otvalor05)
    VALUES
      (pn_IdRemanente_i
      ,n_NmOrdina
      ,aMesaControl(1).NTRAMITE
      ,pv_CdConval_i
      ,pv_Status_i
      ,pd_FESTATUS_i
      ,pv_OtValor01_i
      ,pv_OtValor02_i
      ,pv_OtValor03_i
      ,pv_OtValor04_i
      ,pv_OtValor05_i);
  EXCEPTION
    WHEN OTHERS THEN
      dbms_output.put_line(' ERROR ' || SQLERRM);
  END;

  PROCEDURE P_TVALOREMADET_UPD(pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                              ,pn_NmOrdina_i    IN TVALOREMADET.nmordina%TYPE
                              ,pv_Status_i      IN TVALOREMADET.STATUS%TYPE
                              ,pd_FESTATUS_i    IN TVALOREMADET.FESTATUS%TYPE
                              ,pv_OtValor01_i   IN TVALOREMADET.otvalor01%TYPE
                              ,pv_OtValor02_i   IN TVALOREMADET.otvalor02%TYPE
                              ,pv_OtValor03_i   IN TVALOREMADET.otvalor03%TYPE DEFAULT NULL
                              ,pv_OtValor04_i   IN TVALOREMADET.otvalor04%TYPE DEFAULT NULL
                              ,pv_OtValor05_i   IN TVALOREMADET.otvalor05%TYPE DEFAULT NULL) IS
  BEGIN
    UPDATE tvaloremadet
       SET idremanente = pn_IdRemanente_i
          ,nmordina    = pn_NmOrdina_i
          ,ntramite    = aMesaControl(1).NTRAMITE
          ,status      = pv_Status_i
          ,festatus    = SYSDATE
          ,otvalor01   = pv_OtValor01_i
          ,otvalor02   = pv_OtValor02_i
          ,otvalor03   = pv_OtValor03_i
          ,otvalor04   = pv_OtValor04_i
          ,otvalor05   = pv_OtValor05_i
     WHERE idremanente = pn_IdRemanente_i
       AND nmordina = pn_NmOrdina_i
       AND status   = 'R';
  END;

  PROCEDURE P_TVALOREMADET_STS_UPD(pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                                  ,pn_NmOrdina_i    IN TVALOREMADET.nmordina%TYPE
                                  ,pv_Status_i      IN TVALOREMADET.STATUS%TYPE) IS
  BEGIN
    UPDATE tvaloremadet
       SET status   = pv_Status_i
          ,festatus = SYSDATE
     WHERE idremanente = pn_IdRemanente_i
       AND nmordina = pn_NmOrdina_i;
  END;

  -- --
  PROCEDURE LOAD_CONTRATADO(pn_CdRemanente_i IN TCONFREMA.CDREMANENTE%TYPE
                           ,pv_CdTipoRema_i  IN TCONFREMA.Cdtiporema%TYPE
                           ,pv_CdNivel_i     IN TCONFREMA.CdNivel%TYPE
                           ,pv_CdConval_i    IN TCONFREMA.cdconval%TYPE
                           ,pv_CdTipoGar_i   IN VARCHAR2 DEFAULT 'P'
                           ,pv_UnidadCont_i  IN VARCHAR2 DEFAULT NULL) IS
    CURSOR Cur_Ctto IS
      SELECT owcontra
            ,tacontra
            ,cacontra
            ,T.OWUNCONTRA
            ,T.TAUNCONTRA
            ,T.UNCONTRA
            ,T.CAREMANE
            ,T.UNREMANE
        FROM tconfrema T
       WHERE t.cdramo = aVariables('CDRAMO')
         AND T.CDREMANENTE = pn_CdRemanente_i
         AND T.CDTIPOREMA = pv_CdTipoRema_i
         AND t.cdnivel = pv_CdNivel_i
         AND t.cdconval = pv_CdConval_i
         AND T.SWACTIVO = k_s;
    Reg_Ctto Cur_Ctto%ROWTYPE;
    FUNCTION F_VALOR(pv_Tabla_i IN VARCHAR2
                    ,pv_Campo_i IN VARCHAR2) RETURN VARCHAR2 IS
      v_Return VARCHAR2(100);
    BEGIN
      IF pv_campo_i IS NOT NULL THEN
        CASE pv_Tabla_i
          WHEN 'TVALOSIT' THEN
            BEGIN
              v_Return := aTvalosit(pv_campo_i);
            EXCEPTION
              WHEN no_Data_found THEN
                v_Return := NULL;
                dbms_output.put_line('No se encontro informacion en aTvalosit para el: ' ||
                                     pv_campo_i);
            END;
          WHEN 'TVALOGAR' THEN
            v_Return := aTvalogar(pv_campo_i);
          ELSE
            v_return := NULL;
        END CASE;
      ELSE
        v_return := NULL;
      END IF;
      RETURN v_return;
    END;
  
  BEGIN
    OPEN CUR_CTTO;
    LOOP
      FETCH CUR_CTTO
        INTO REG_CTTO;
      EXIT WHEN cur_ctto%NOTFOUND;
      IF Reg_Ctto.owcontra = 'ICE' AND REG_CTTO.tacontra = 'MPOLICAP' THEN
        IF pv_CdTipoGar_i != 'P' THEN
          aRemanente(pn_CdRemanente_i)('OTVALOR01') := aCoberturaAux(1).nSumaAsegurada;
          aRemanente(pn_CdRemanente_i)('OTVALOR02') := aCoberturaAux(1).vMoneda;
        ELSE
          aRemanente(pn_CdRemanente_i)('OTVALOR01') := aCobertura(1).nSumaAsegurada;
          aRemanente(pn_CdRemanente_i)('OTVALOR02') := aCobertura(1).vMoneda;
        END IF;
      ELSE
        aRemanente(pn_CdRemanente_i)(REG_CTTO.CAREMANE) := F_VALOR(Reg_Ctto.Tacontra
                                                                  ,Reg_Ctto.Cacontra);
        IF REG_CTTO.UNREMANE IS NOT NULL THEN
          aRemanente(pn_CdRemanente_i)(REG_CTTO.UNREMANE) := nvl(F_VALOR(Reg_Ctto.Tacontra
                                                                        ,Reg_Ctto.Uncontra)
                                                                ,pv_UnidadCont_i);
        END IF;
      END IF;
    END LOOP;
    CLOSE CUR_CTTO;
  END;

  PROCEDURE p_tvaloval_ins(pv_cdgarant_i   IN tvaloval.cdgarant%TYPE
                          ,pv_cdtipval_i   IN tvaloval.cdtipval%TYPE
                          ,pv_cdconval_i   IN tvaloval.cdconval%TYPE
                          ,pv_cdconcep_i   IN tvaloval.cdconcep%TYPE
                          ,pv_userregi_i   IN tvaloval.userregi%TYPE
                          ,pd_feregist_i   IN tvaloval.feregist%TYPE
                          ,pn_nmcantapr_i  IN NUMBER DEFAULT NULL
                          ,pv_UnidadCant_i IN VARCHAR2
                          ,pv_CdTipo_i     IN VARCHAR2) IS
    -- --
    n_existe   PLS_INTEGER := 0;
    n_caafecta PLS_INTEGER := 0;
    v_unafecta VARCHAR2(30);
    v_sql      VARCHAR2(2000) := NULL;
    -- 
    -- --
    CURSOR Cur_Configura IS
      SELECT tco.cdtiporema
            ,tco.owafecta || '.' || tco.taafecta AS tab_afecta
            ,tco.caafecta AS caafecta
            ,CASE
               WHEN tco.owunafecta IS NOT NULL AND
                    tco.taunafecta IS NOT NULL THEN
                tco.owunafecta || '.' || tco.taunafecta
               ELSE
                NULL
             END AS tab_unafec
            ,tco.taunafecta
            ,tco.unafecta AS unafecta
        FROM tconfrema tco
       WHERE tco.cdramo = aMesaControl(1).CdRamo
         AND tco.cdconval = pv_cdconval_i
         AND tco.cdconval NOT IN
             (SELECT otclave1
                FROM ttapvaat tv
               WHERE tv.otclave1 = aVTMesaControl('MODALIDAD DEL TRAMITE')
                 AND tv.nmtabla =
                     (SELECT tt.nmtabla
                        FROM ttaptabl tt
                       WHERE tt.cdtabla = 'DETRAMIVAL')
               GROUP BY otclave1)
         AND tco.cdtiporema = 'CO'
         AND TCO.SWACTIVO = K_S
       ORDER BY tco.cdtiporema
               ,tco.cdremanente ASC;
    -- --
    Reg_Configura Cur_Configura%ROWTYPE;
    -- --
  BEGIN
    -- --
    OPEN Cur_Configura;
    LOOP
      FETCH Cur_Configura
        INTO Reg_Configura;
      EXIT WHEN Cur_Configura%NOTFOUND;
      -- --
      IF nvl(Reg_Configura.cdtiporema
            ,'XX') = 'CO' THEN
        IF pv_cdtipo_i != 'P' THEN
          n_caafecta := NULL;
          v_unafecta := NULL;
        ELSE
          n_caafecta := pn_nmcantapr_i;
          v_unafecta := pv_UnidadCant_i;
        END IF;
      END IF;
      -- --
      BEGIN
        v_sql := ' select 1 from  ' || Reg_Configura.tab_afecta ||
                 ' xx  
                  where xx.ntramite  = :1 
                    and xx.cdunieco  = :2
                    and xx.cdramo    = :3
                    and xx.aaapertu  = :4
                    and xx.status    = :5
                    and xx.nmsinies  = :6
                    and xx.cdgarant  = :7
                    and xx.cdconcep  = :8
                    and xx.cdconval  = :9
                    and xx.cdtipval  = :10';
        --        dbms_output.put_line('*1*' || v_sql);
        EXECUTE IMMEDIATE v_sql
          INTO n_existe
          USING aMesaControl(1).NTramite, aMesaControl(1).cdunieco, aMesaControl(1).cdramo, aSiniestro(1).aaapertu, aSiniestro(1).status, aSiniestro(1).nmsinies, pv_cdgarant_i, pv_cdconcep_i, pv_cdconval_i, pv_cdtipval_i;
      EXCEPTION
        WHEN no_data_found THEN
          n_existe := 0;
      END;
      -- --
      IF nvl(n_existe
            ,0) = 1 THEN
        -- --
        IF Reg_Configura.Unafecta IS NOT NULL THEN
          v_sql := ' update ' || Reg_Configura.tab_afecta || ' xx ';
          v_sql := v_sql || 'set xx.' || Reg_Configura.caafecta || '=' ||
                   CHR(39) || n_caafecta || CHR(39);
          v_sql := v_sql || ',' || Reg_Configura.unafecta || '=' || chr(39) ||
                   v_unafecta || chr(39);
          v_sql := v_sql || ' where xx.ntramite  = :1';
          v_sql := v_sql || ' and xx.cdunieco    = :2';
          v_sql := v_sql || ' and xx.cdramo      = :3';
          v_sql := v_sql || ' and xx.aaapertu    = :4';
          v_sql := v_sql || ' and xx.status      = :5';
          v_sql := v_sql || ' and xx.nmsinies    = :6';
          v_sql := v_sql || ' and xx.cdgarant    = :7';
          v_sql := v_sql || ' and xx.cdconcep    = :8';
          v_sql := v_sql || ' and xx.cdconval    = :9';
          v_sql := v_sql || ' and xx.cdtipval    = :10';
          --          dbms_output.put_line('*2*' || v_sql);
          EXECUTE IMMEDIATE v_sql
            USING aMesaControl(1).NTramite, aMesaControl(1).cdunieco, aMesaControl(1).cdramo, aSiniestro(1).aaapertu, aSiniestro(1).status, aSiniestro(1).nmsinies, pv_cdgarant_i, pv_cdconcep_i, pv_cdconval_i, pv_cdtipval_i;
        ELSE
          v_sql := ' update ' || Reg_Configura.tab_afecta || ' xx ';
          v_sql := v_sql || 'set xx.' || Reg_Configura.caafecta || '=' ||
                   CHR(39) || n_caafecta || CHR(39);
          v_sql := v_sql || ' where xx.ntramite  = :1';
          v_sql := v_sql || ' and xx.cdunieco    = :2';
          v_sql := v_sql || ' and xx.cdramo      = :3';
          v_sql := v_sql || ' and xx.aaapertu    = :4';
          v_sql := v_sql || ' and xx.status      = :5';
          v_sql := v_sql || ' and xx.nmsinies    = :6';
          v_sql := v_sql || ' and xx.cdgarant    = :7';
          v_sql := v_sql || ' and xx.cdconcep    = :8';
          v_sql := v_sql || ' and xx.cdconval    = :9';
          v_sql := v_sql || ' and xx.cdtipval    = :10';
          --          dbms_output.put_line('*2*' || v_sql);
          EXECUTE IMMEDIATE v_sql
            USING aMesaControl(1).NTramite, aMesaControl(1).cdunieco, aMesaControl(1).cdramo, aSiniestro(1).aaapertu, aSiniestro(1).status, aSiniestro(1).nmsinies, pv_cdgarant_i, pv_cdconcep_i, pv_cdconval_i, pv_cdtipval_i;
        END IF;
        -- --
      ELSIF nvl(n_existe
               ,0) = 0 THEN
        -- --
        IF Reg_Configura.Unafecta IS NOT NULL THEN
          v_sql := 'insert into ' || Reg_Configura.tab_afecta;
          v_sql := v_Sql || '(ntramite, cdunieco, cdramo, aaapertu, status, nmsinies, cdgarant, cdtipval, cdconval, cdconcep
                     , userregi, feregist, ' ||
                   Reg_Configura.Caafecta || ',' || Reg_Configura.Unafecta || ')';
          v_sql := v_Sql ||
                   'values (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14)';
          --          dbms_output.put_line('*3*' || v_sql);
          EXECUTE IMMEDIATE v_sql
            USING aMesaControl(1).NTramite, aMesaControl(1).cdunieco, aMesaControl(1).cdramo, aSiniestro(1).aaapertu, aSiniestro(1).status, aSiniestro(1).nmsinies, pv_cdgarant_i, pv_cdtipval_i, pv_cdconval_i, pv_cdconcep_i, pv_userregi_i, pd_feregist_i, n_caafecta, v_unafecta;
        ELSE
          v_sql := 'insert into ' || Reg_Configura.tab_afecta;
          v_sql := v_Sql || '(ntramite, cdunieco, cdramo, aaapertu, status, nmsinies, cdgarant, cdtipval, cdconval, cdconcep
                     , userregi, feregist, ' ||
                   Reg_Configura.Caafecta || ')';
          v_sql := v_Sql ||
                   'values (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13)';
          --          dbms_output.put_line('*3*' || v_sql);
          EXECUTE IMMEDIATE v_sql
            USING aMesaControl(1).NTramite, aMesaControl(1).cdunieco, aMesaControl(1).cdramo, aSiniestro(1).aaapertu, aSiniestro(1).status, aSiniestro(1).nmsinies, pv_cdgarant_i, pv_cdtipval_i, pv_cdconval_i, pv_cdconcep_i, pv_userregi_i, pd_feregist_i, n_caafecta;
        END IF;
        -- --
      END IF;
      -- --
    END LOOP;
    CLOSE Cur_Configura;
  END;

  --
  PROCEDURE P_TVALOVAL_TSUBDETFACT_CONT_INS(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                                           ,pv_usuario_i  IN VARCHAR2 DEFAULT NULL) IS
    -- --
    CURSOR cur_tsubdetfact IS
      SELECT tde.cdconcepto AS cdconcep
            ,nvl(SUM(tsdf.valor)
                ,0) AS nmcantapr -- * nvl((select nvl(otvalor15,1) from ttapvaat where nmtabla=(select nmtabla from ttaptabl where cdtabla='999MCPT') and otclave1 = tde.cdcpt),1)as nmcantapr 
            ,UNI.OTVALOR AS DSUNIDAD
            ,tsdf.CDCONVAL
            ,TV.OTVALOR AS CDTIPVAL
        FROM tdetfact tde
            ,tfactura tfa
            ,TSUBDETFACT tsdf
            ,(SELECT OTCLAVE1 AS CDCONCEPTO
                    ,OTVALOR
                FROM VTtapvaat T
               WHERE NMTABLA IN (SELECT NMTABLA
                                   FROM TTAPTABL
                                  WHERE CDTABLA = 'CONCGAST')
                 AND DSATRIBU = 'UNIDADES') UNI
            ,(SELECT OTCLAVE1 AS CDCONCEPTO
                    ,OTVALOR
                FROM VTtapvaat T
               WHERE NMTABLA IN (SELECT NMTABLA
                                   FROM TTAPTABL
                                  WHERE CDTABLA = 'CONCGAST')
                 AND dsatribu = 'TIPO DE VALORACION') TV
       WHERE tfa.ntramite = pn_ntramite_i
         AND tfa.ntramite = tde.ntramite
         AND tfa.cdunieco = tde.cdunieco
         AND tfa.cdramo = tde.cdramo
         AND tfa.aaapertu = tde.aaapertu
         AND tfa.nmsinies = tde.nmsinies
         AND tfa.nmfactura = tde.nmfactura
         AND tde.CDUNIECO = tsdf.CDUNIECO
         AND tde.CDRAMO = tsdf.CDRAMO
         AND tde.AAAPERTU = tsdf.AAAPERTU
         AND tde.NMSINIES = tsdf.NMSINIES
         AND tde.NMFACTURA = tsdf.NMFACTURA
         AND tde.NMORDFAC = tsdf.NMORDFAC
         AND tde.NTRAMITE = tsdf.NTRAMITE
         AND UNI.CDCONCEPTO = tsdf.CDCONCEPTO
         AND TV.CDCONCEPTO = tsdf.CDCONCEPTO
       GROUP BY tde.CDCONCEPTO
               ,tsdf.CDCONVAL
               ,TFA.CDMONEDA
               ,UNI.OTVALOR
               ,TV.OTVALOR
               ,tde.cdcpt;
    reg_tfactura cur_tsubdetfact %ROWTYPE;
    -- --
    CURSOR cur_vcptgasto IS
      SELECT tsi.cdgarant AS cdgarant
            ,tsi.cdtipo   AS cdtipo
        FROM tsinigar tsi
       WHERE tsi.cdunieco = amesacontrol(1).cdunieco
         AND tsi.cdramo = amesacontrol(1).cdramo
         AND tsi.ntramite = pn_ntramite_i
         AND EXISTS (SELECT NULL
                FROM tragacov tra
               WHERE tra.cdgarant = tsi.cdgarant
                 AND tra.cdramo = tsi.cdramo
                 AND tra.cdconval = reg_tfactura.cdconval
                 AND tra.cdtipval = reg_tfactura.cdtipval)
         AND EXISTS (SELECT NULL
                FROM tconfrema t
               WHERE t.cdconval = reg_tfactura.cdconval
                 AND t.owafecta = 'SINIESTROS'
                 AND t.taafecta = 'TVALOVAL'
                 AND T.SWACTIVO = K_S
                 AND t.cdramo = tsi.cdramo);
    -- --
    reg_vcptgasto cur_vcptgasto%ROWTYPE;
    -- --
  BEGIN
    load_datos(pn_ntramite_i);
    -- --
    OPEN cur_tsubdetfact;
    LOOP
      FETCH cur_tsubdetfact
        INTO reg_tfactura;
      EXIT WHEN cur_tsubdetfact%NOTFOUND;
      OPEN cur_vcptgasto;
      LOOP
        FETCH cur_vcptgasto
          INTO reg_vcptgasto;
        EXIT WHEN cur_vcptgasto%NOTFOUND;
        p_tvaloval_ins(pv_cdgarant_i   => reg_vcptgasto.cdgarant
                      ,pv_cdtipval_i   => reg_tfactura.cdtipval
                      ,pv_cdconval_i   => reg_tfactura.cdconval
                      ,pv_cdconcep_i   => reg_tfactura.cdconcep
                      ,pv_userregi_i   => nvl(pv_usuario_i
                                             ,USER)
                      ,pd_feregist_i   => SYSDATE
                      ,pn_nmcantapr_i  => reg_tfactura.nmcantapr
                      ,pv_unidadcant_i => reg_tfactura.dsunidad
                      ,pv_cdtipo_i     => reg_vcptgasto.cdtipo);
        -- --
      END LOOP;
      CLOSE cur_vcptgasto;
    END LOOP;
    CLOSE cur_tsubdetfact;
    -- --
  END;

  --
  PROCEDURE P_TVALOVAL_FACTURA_CONT_INS(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                                       ,pv_usuario_i  IN VARCHAR2 DEFAULT NULL) IS
    -- --
    CURSOR cur_tfactura IS
      SELECT tde.cdconcepto AS cdconcep
            ,nvl(SUM(tde.nmcantapr)
                ,0) * CASE
               WHEN tde.cdcpt = '00000' THEN
                '1'
               ELSE
                nvl((SELECT nvl(otvalor15
                              ,1)
                      FROM ttapvaat
                     WHERE nmtabla = (SELECT nmtabla
                                        FROM ttaptabl
                                       WHERE cdtabla = '999MCPT')
                       AND otclave1 = tde.cdcpt)
                   ,1)
             END AS nmcantapr
            ,UNI.OTVALOR AS DSUNIDAD
            ,cval.otvalor AS cdconval
            ,TV.OTVALOR AS CDTIPVAL
        FROM tdetfact tde
            ,tfactura tfa
            ,(SELECT OTCLAVE1 AS CDCONCEPTO
                    ,OTVALOR
                FROM VTtapvaat T
               WHERE NMTABLA IN (SELECT NMTABLA
                                   FROM TTAPTABL
                                  WHERE CDTABLA = 'CONCGAST')
                 AND DSATRIBU = 'UNIDADES') UNI
            ,(SELECT OTCLAVE1 AS CDCONCEPTO
                    ,OTVALOR
                FROM VTtapvaat T
               WHERE NMTABLA IN (SELECT NMTABLA
                                   FROM TTAPTABL
                                  WHERE CDTABLA = 'CONCGAST')
                 AND dsatribu = 'CDCONVAL') CVAL
            ,(SELECT OTCLAVE1 AS CDCONCEPTO
                    ,OTVALOR
                FROM VTtapvaat T
               WHERE NMTABLA IN (SELECT NMTABLA
                                   FROM TTAPTABL
                                  WHERE CDTABLA = 'CONCGAST')
                 AND dsatribu = 'MANEJA CONTADORES') CON
            ,(SELECT OTCLAVE1 AS CDCONCEPTO
                    ,OTVALOR
                FROM VTtapvaat T
               WHERE NMTABLA IN (SELECT NMTABLA
                                   FROM TTAPTABL
                                  WHERE CDTABLA = 'CONCGAST')
                 AND dsatribu = 'TIPO DE VALORACION') TV
       WHERE tfa.ntramite = pn_ntramite_i
         AND tfa.ntramite = tde.ntramite
         AND tfa.cdunieco = tde.cdunieco
         AND tfa.cdramo = tde.cdramo
         AND tfa.aaapertu = tde.aaapertu
         AND tfa.nmsinies = tde.nmsinies
         AND tfa.nmfactura = tde.nmfactura
         AND UNI.CDCONCEPTO = TDE.CDCONCEPTO
         AND cval.cdconcepto = tde.cdconcepto
         AND CON.cdconcepto = tde.cdconcepto
         AND CON.OTVALOR = 'SI'
         AND TV.CDCONCEPTO = TDE.CDCONCEPTO
         AND cval.otvalor NOT IN
             (SELECT OTCLAVE3 AS CDCONVAL
                FROM Ttapvaat T
               WHERE NMTABLA = (SELECT NMTABLA
                                  FROM TTAPTABL
                                 WHERE CDTABLA = 'CONVALXCONCEP'))
       GROUP BY tde.cdconcepto
               ,TFA.CDMONEDA
               ,UNI.OTVALOR
               ,cval.otvalor
               ,TV.OTVALOR
               ,tde.cdcpt;
    reg_tfactura cur_tfactura %ROWTYPE;
    -- --
    CURSOR cur_vcptgasto IS
      SELECT tsi.cdgarant AS cdgarant
            ,tsi.cdtipo   AS cdtipo
        FROM tsinigar tsi
       WHERE tsi.cdunieco = amesacontrol(1).cdunieco
         AND tsi.cdramo = amesacontrol(1).cdramo
         AND tsi.ntramite = pn_ntramite_i
         AND EXISTS (SELECT NULL
                FROM tragacov tra
               WHERE tra.cdgarant = tsi.cdgarant
                 AND tra.cdramo = tsi.cdramo
                 AND tra.cdconval = reg_tfactura.cdconval
                 AND tra.cdtipval = reg_tfactura.cdtipval)
         AND EXISTS (SELECT NULL
                FROM tconfrema t
               WHERE t.cdconval = reg_tfactura.cdconval
                 AND t.owafecta = 'SINIESTROS'
                 AND t.taafecta = 'TVALOVAL'
                 AND t.SWACTIVO = K_S
                 AND t.cdramo = tsi.cdramo);
    -- --
    reg_vcptgasto cur_vcptgasto%ROWTYPE;
    -- --
  BEGIN
    load_datos(pn_ntramite_i);
    p_tvaloval_tsubdetfact_cont_ins(pn_ntramite_i
                                   ,pv_usuario_i);
    -- --
    OPEN cur_tfactura;
    LOOP
      FETCH cur_tfactura
        INTO reg_tfactura;
      EXIT WHEN cur_tfactura%NOTFOUND;
      OPEN cur_vcptgasto;
      LOOP
        FETCH cur_vcptgasto
          INTO reg_vcptgasto;
        EXIT WHEN cur_vcptgasto%NOTFOUND;
        p_tvaloval_ins(pv_cdgarant_i   => reg_vcptgasto.cdgarant
                      ,pv_cdtipval_i   => reg_tfactura.cdtipval
                      ,pv_cdconval_i   => reg_tfactura.cdconval
                      ,pv_cdconcep_i   => reg_tfactura.cdconcep
                      ,pv_userregi_i   => nvl(pv_usuario_i
                                             ,USER)
                      ,pd_feregist_i   => SYSDATE
                      ,pn_nmcantapr_i  => reg_tfactura.nmcantapr
                      ,pv_unidadcant_i => reg_tfactura.dsunidad
                      ,pv_cdtipo_i     => reg_vcptgasto.cdtipo);
        -- --
      END LOOP;
      CLOSE cur_vcptgasto;
    END LOOP;
    CLOSE cur_tfactura;
    -- --
  END;

  PROCEDURE P_TVALOVAL_PROGRAM_CONT_INS(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                                       ,pv_usuario_i  IN VARCHAR2 DEFAULT NULL) IS
    -- --
    CURSOR cur_programacion IS
      SELECT pc.ntramite
            ,pc.cdconcep
            ,pc.dsunidad
            ,co.concepto_remanente
            ,co.cdtipval
            ,SUM(pc.nmcantapr) AS nmcantapr
        FROM vw_programacion_cptos pc
        JOIN VW_CLASPROG_CONT co
          ON co.concepto_gasto = pc.cdconcep
       WHERE ntramite = pn_ntramite_i
       GROUP BY pc.ntramite
               ,pc.cdconcep
               ,pc.dsunidad
               ,co.concepto_remanente
               ,co.cdtipval;
    -- --
    reg_programacion cur_programacion %ROWTYPE;
    -- --
    CURSOR cur_vcptgasto IS
      SELECT tsi.cdgarant AS cdgarant
            ,tsi.cdtipo   AS cdtipo
        FROM tsinigar tsi
       WHERE tsi.cdunieco = amesacontrol(1).cdunieco
         AND tsi.cdramo = amesacontrol(1).cdramo
         AND tsi.ntramite = pn_ntramite_i
         AND EXISTS
       (SELECT NULL
                FROM tragacov tra
               WHERE tra.cdgarant = tsi.cdgarant
                 AND tra.cdramo = tsi.cdramo
                 AND tra.cdconval = reg_programacion.concepto_remanente
                 AND tra.cdtipval = reg_programacion.cdtipval)
         AND EXISTS
       (SELECT NULL
                FROM tconfrema t
               WHERE t.cdconval = reg_programacion.concepto_Remanente
                 AND t.owafecta = 'SINIESTROS'
                 AND t.taafecta = 'TVALOVAL'
                 AND t.swactivo = k_S
                 AND t.cdramo = tsi.cdramo);
    -- --
    reg_vcptgasto cur_vcptgasto%ROWTYPE;
    -- --
  BEGIN
    load_datos(pn_ntramite_i);
    -- --
    OPEN cur_programacion;
    LOOP
      FETCH cur_programacion
        INTO reg_programacion;
      EXIT WHEN cur_programacion%NOTFOUND;
      -- --
      OPEN cur_vcptgasto;
      LOOP
        FETCH cur_vcptgasto
          INTO reg_vcptgasto;
        EXIT WHEN cur_vcptgasto%NOTFOUND;
        -- --
        -- --
        p_tvaloval_ins(pv_cdgarant_i   => reg_vcptgasto.cdgarant
                      ,pv_cdtipval_i   => reg_programacion.cdtipval
                      ,pv_cdconval_i   => reg_programacion.concepto_Remanente
                      ,pv_cdconcep_i   => reg_programacion.cdconcep
                      ,pv_userregi_i   => nvl(pv_usuario_i
                                             ,USER)
                      ,pd_feregist_i   => SYSDATE
                      ,pn_nmcantapr_i  => reg_programacion.nmcantapr
                      ,pv_unidadcant_i => reg_programacion.dsunidad
                      ,pv_cdtipo_i     => reg_vcptgasto.cdtipo);
        -- --
      END LOOP;
      CLOSE cur_vcptgasto;
    END LOOP;
    CLOSE cur_programacion;
    -- --
  END;

  PROCEDURE P_GENERAR_TVALOVAL(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
    i_Modalidad PLS_INTEGER;
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    i_Modalidad := TO_NUMBER(NVL(aVTMesaControl('MODALIDAD DEL TRAMITE')
                                ,0));
    CASE
      WHEN ((nvl(i_Modalidad
                ,0) = 1) AND nvl(aMesaControl(1).otvalor10
                                 ,'0') = '0') THEN
        P_TVALOVAL_FACTURA_CONT_INS(pn_ntramite_i);
      ELSE
        P_TVALOVAL_PROGRAM_CONT_INS(pn_ntramite_i);
    END CASE;
  END;

  PROCEDURE P_APROBAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
    i_Modalidad PLS_INTEGER;
    v_Status    VARCHAR2(1);
    CURSOR Cur_D IS
      SELECT D.idremanente
            ,D.NMORDINA
            ,c.cdtiporema
        FROM tvaloremadet d
        JOIN tvalorema r
          ON r.idremanente = d.idremanente
        JOIN tconfrema c
          ON c.cdremanente = r.cdremanente
       WHERE d.ntramite = pn_ntramite_i
         AND C.SWACTIVO = K_S
         AND d.status = 'D';
    Reg_D Cur_D%ROWTYPE;
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    i_Modalidad := TO_NUMBER(NVL(aVTMesaControl('MODALIDAD DEL TRAMITE')
                                ,0));
    CASE
      WHEN i_modalidad IN (1
                          ,3
                          ,4) THEN
        v_Status := 'P';
      WHEN i_modalidad IN (5
                          ,2
                          ,6
                          ,7
                          ,8) THEN
        v_Status := 'T';
      ELSE
        v_Status := NULL;
    END CASE;
    IF v_Status IS NOT NULL THEN
      OPEN Cur_D;
      LOOP
        FETCH Cur_D
          INTO Reg_D;
        EXIT WHEN Cur_D%NOTFOUND;
        IF Reg_D.Cdtiporema = 'CO' THEN
          UPDATE TVALOREMADET D
             SET D.STATUS   = 'P'
                ,d.feSTATUS = SYSDATE
           WHERE d.idremanente = Reg_d.idremanente
             AND d.nmordina = Reg_d.Nmordina;
        ELSE
          UPDATE TVALOREMADET D
             SET D.STATUS   = V_Status
                ,d.feSTATUS = SYSDATE
           WHERE d.idremanente = Reg_d.idremanente
             AND d.nmordina = Reg_d.Nmordina;
        END IF;
      END LOOP;
      CLOSE Cur_D;
    END IF;
  END;

  FUNCTION F_REMANENTE_GET(pn_ntramite_i    IN tmesacontrol.ntramite%TYPE
                          ,pv_CdNivel_i     IN VARCHAR2
                          ,pv_cdtiporema_i  IN VARCHAR2
                          ,pv_PorTramite_i  IN VARCHAR2
                          ,pv_Status_i      IN VARCHAR2
                          ,pv_cdtipcon_i    IN VARCHAR2
                          ,pv_Unidad_o      OUT VARCHAR2
                          ,pv_nmordina_o    OUT number
                          ,pn_cdRemanente_i IN PLS_INTEGER DEFAULT NULL
                          ,pv_CdTipo_i      IN VARCHAR2 DEFAULT 'P'
                          ,pn_IdRemanente_i IN PLS_INTEGER DEFAULT NULL)
    RETURN NUMBER IS
    v_SQL VARCHAR2(4000);
    CURSOR Cur_Tatrirema(pv_TpField_i IN VARCHAR2) IS
      SELECT CDATRIBU
            ,IDATRIBU
        FROM TATRIREMA
       WHERE CDNIVEL = pv_CdNivel_i
         AND tpfield = pv_TpField_i
       ORDER BY CDATRIBU;
    Reg_Tatrirema  Cur_Tatrirema%ROWTYPE;
    V_SQL_G        VARCHAR2(4000);
    n_Return       NUMBER(14
                         ,6);
    b_Existe_Grupo BOOLEAN;
  BEGIN
    v_CdTipo := nvl(pv_CdTipo_i
                   ,'P');
    LOAD_DATOS(pn_ntramite_i);
    IF pn_IdRemanente_i IS NULL OR pv_cdtipcon_i IS NOT NULL THEN
      v_SQL := 'SELECT ';
      OPEN Cur_Tatrirema('A');
      LOOP
        FETCH Cur_Tatrirema
          INTO Reg_Tatrirema;
        EXIT WHEN Cur_Tatrirema%NOTFOUND;
        IF Reg_Tatrirema.IDATRIBU = 'VALOR' THEN
          v_SQL := v_SQL || 'SUM(NVL(' ||
                   TRIM('OTVALOR' || TRIM(TO_CHAR(Reg_Tatrirema.Cdatribu
                                                 ,'00'))) || ',0)) AS ' || Reg_Tatrirema.IDATRIBU || ',';
        ELSE
          v_SQL := v_SQL ||
                   TRIM('OTVALOR' || TRIM(TO_CHAR(Reg_Tatrirema.Cdatribu
                                                 ,'00'))) || ' AS ' ||
                   Reg_Tatrirema.IDATRIBU || ',';
        END IF;
      END LOOP;
      CLOSE cur_tatrirema;
      V_SQL :=  V_SQL || 'MAX(RD.nmordina) as nmordina';
      --
      --
      v_SQL := v_SQL || ' FROM TVALOREMADET RD ';
      --
      v_SQL := v_SQL || 'WHERE RD.STATUS = :1 ';
      IF pv_cdtipcon_i IS NOT NULL THEN
        CASE pv_cdtipcon_i
          WHEN 'GHOSP' THEN
            v_SQL := v_SQL ||
                     ' AND RD.cdconval IN (SELECT CDCONVAL FROM TCONCSIN T ' ||
                     ' WHERE T.CDTIPCON in (' || CHR(39) || 'GHOSP' ||
                     CHR(39) || ',' || CHR(39) || 'DIGHO' || CHR(39) || ',' ||
                     CHR(39) || 'DNGHO' || CHR(39) || ')' || ')';
          WHEN 'HMED' THEN
            v_SQL := v_SQL ||
                     ' AND RD.cdconval IN (SELECT CDCONVAL FROM TCONCSIN T ' ||
                     ' WHERE T.CDTIPCON in (' || CHR(39) || 'HMED' ||
                     CHR(39) || ',' || CHR(39) || 'TPCO' || CHR(39) || ')' || ')';
          ELSE
            v_SQL := v_SQL ||
                     ' AND RD.cdconval IN (SELECT CDCONVAL FROM TCONCSIN T WHERE T.CDTIPCON =' ||
                     CHR(39) || pv_cdtipcon_i || chr(39) || ')';
        END CASE;
      END IF;
      IF pv_PorTramite_i = k_s THEN
        v_SQL := v_SQL || ' AND RD.NTRAMITE = ' || pn_ntramite_i;
      END IF;
      v_SQL := v_SQL || ' AND EXISTS 
             (SELECT NULL FROM TVALOREMA R 
             WHERE (R.CDNIVEL = :2  OR :3 IS NULL)
             AND R.IDREMANENTE = RD.IDREMANENTE';
      IF pn_cdRemanente_i IS NOT NULL THEN
        v_SQL := v_SQL || ' AND R.cdremanente = ' || pn_cdRemanente_i;
      END IF;
      IF pn_IdRemanente_i IS NOT NULL AND pv_cdtipcon_i IS NULL THEN
        v_SQL := v_SQL || ' AND R.idremanente = ' || pn_IdRemanente_i;
      END IF;
      OPEN Cur_Tatrirema('C');
      LOOP
        FETCH Cur_Tatrirema
          INTO Reg_Tatrirema;
        EXIT WHEN Cur_Tatrirema%NOTFOUND;
        v_SQL := v_SQL || ' AND OTCLAVE' || Reg_Tatrirema.Cdatribu || '=' ||
                 CHR(39) || aVariables(Reg_Tatrirema.Idatribu) || CHR(39);
      END LOOP;
      CLOSE cur_tatrirema;
      V_SQL := V_SQL || ' AND EXISTS (SELECT NULL
                  FROM TCONFREMA T
                 WHERE T.CDRAMO = R.OTCLAVE2
                   AND T.CDREMANENTE = R.CDREMANENTE
                   AND T.CDNIVEL = R.CDNIVEL                   
                   AND T.cdtiporema = :4
                   and T.SWACTIVO = :5))';
      OPEN Cur_Tatrirema('A');
      LOOP
        FETCH Cur_Tatrirema
          INTO Reg_Tatrirema;
        EXIT WHEN Cur_Tatrirema%NOTFOUND;
        IF Reg_Tatrirema.IDATRIBU != 'VALOR' THEN
          b_Existe_Grupo := TRUE;
          V_SQL_G        := V_SQL_G || TRIM('OTVALOR' ||
                                            TRIM(TO_CHAR(Reg_Tatrirema.Cdatribu
                                                        ,'00'))) || ',';
        END IF;
      END LOOP;
      CLOSE cur_tatrirema;
      IF b_Existe_Grupo THEN
        V_SQL_G := V_SQL_G || '*';
        V_SQL_G := REPLACE(V_SQL_G
                          ,',*');
        IF V_SQL_G IS NOT NULL THEN
          V_SQL := V_SQL || 'GROUP BY ' || V_SQL_G;
        END IF;
      END IF;
      dbms_output.put_line(v_sql);
      BEGIN
        IF pv_PorTramite_i = k_s THEN
          EXECUTE IMMEDIATE v_sql
            INTO n_Return, pv_Unidad_o, pv_nmordina_o
            USING pv_Status_i, pv_CdNivel_i, pv_CdNivel_i, pv_cdtiporema_i, k_s;
        ELSE
          EXECUTE IMMEDIATE v_sql
            INTO n_Return, pv_Unidad_o, pv_nmordina_o
            USING pv_Status_i, pv_CdNivel_i, pv_CdNivel_i, pv_cdtiporema_i, k_s;
        END IF;
      EXCEPTION
        WHEN no_Data_found THEN
          n_Return    := 0;
          pv_Unidad_o := NULL;
          pv_nmordina_o := NULL;
      END;
    ELSE
      BEGIN
        SELECT nvl(SUM(nvl(otvalor01
                          ,0))
                  ,0)
              ,otvalor02
              ,MAX(nmordina)
          INTO n_Return
              ,pv_Unidad_o
              ,pv_nmordina_o
          FROM TVALOREMADET D
         WHERE D.IDREMANENTE = pn_IdRemanente_i
           AND d.status = pv_Status_i
         GROUP BY otvalor02;
         
      EXCEPTION
        WHEN OTHERS THEN
          n_Return    := 0;
          pv_Unidad_o := NULL;
      END;
    END IF;
    RETURN n_Return;
  END;

  FUNCTION F_REMANENTE_FINAL_GET(pn_ntramite_i           IN tmesacontrol.ntramite%TYPE
                                ,pv_CdNivel_i            IN VARCHAR2
                                ,pv_cdtiporema_i         IN VARCHAR2
                                ,pv_cdtipcon_i           IN VARCHAR2
                                ,pv_SumarTramiteActual_i IN VARCHAR2
                                ,pv_Unidad_o             OUT VARCHAR2
                                ,pv_IncluyeTransito_i    IN VARCHAR2 DEFAULT 'N'
                                ,pv_CdTipo_i             IN VARCHAR2 DEFAULT 'P')
    RETURN NUMBER IS
    pv_UnidadC   VARCHAR2(5);
    n_Return     NUMBER := 0;
    n_Contratado NUMBER := 0;
    n_Pagado     NUMBER := 0;
    n_Transito   NUMBER := 0;
    n_Dictamen   NUMBER := 0;
    n_nmordina   NUMBER := 0;
  BEGIN
    n_Contratado := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                   ,pv_CdNivel_i     => pv_CdNivel_i
                                   ,pv_cdtiporema_i  => pv_cdtiporema_i
                                   ,pv_PorTramite_i  => 'N'
                                   ,pv_Status_i      => 'R'
                                   ,pv_cdtipcon_i    => pv_cdtipcon_i
                                   ,pv_Unidad_o      => pv_UnidadC
                                   ,pv_nmordina_o    => n_nmordina
                                   ,pn_cdRemanente_i => NULL
                                   ,pv_CdTipo_i      => pv_CdTipo_i);
    n_Pagado     := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                   ,pv_CdNivel_i     => pv_CdNivel_i
                                   ,pv_cdtiporema_i  => pv_cdtiporema_i
                                   ,pv_PorTramite_i  => 'N'
                                   ,pv_Status_i      => 'P'
                                   ,pv_cdtipcon_i    => pv_cdtipcon_i
                                   ,pv_Unidad_o      => pv_Unidad_o
                                   ,pv_nmordina_o    => n_nmordina
                                   ,pn_cdRemanente_i => NULL
                                   ,pv_CdTipo_i      => pv_CdTipo_i);
    IF pv_IncluyeTransito_i = k_s THEN
      n_Transito := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                   ,pv_CdNivel_i     => pv_CdNivel_i
                                   ,pv_cdtiporema_i  => pv_cdtiporema_i
                                   ,pv_PorTramite_i  => 'N'
                                   ,pv_Status_i      => 'T'
                                   ,pv_cdtipcon_i    => pv_cdtipcon_i
                                   ,pv_Unidad_o      => pv_Unidad_o
                                   ,pv_nmordina_o    => n_nmordina
                                   ,pn_cdRemanente_i => NULL
                                   ,pv_CdTipo_i      => pv_CdTipo_i);
    END IF;
    n_Transito := nvl(n_Transito
                     ,0);
    IF pv_SumarTramiteActual_i = k_s THEN
      n_Dictamen := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                   ,pv_CdNivel_i     => pv_CdNivel_i
                                   ,pv_cdtiporema_i  => pv_cdtiporema_i
                                   ,pv_PorTramite_i  => k_s
                                   ,pv_Status_i      => 'D'
                                   ,pv_cdtipcon_i    => pv_cdtipcon_i
                                   ,pv_Unidad_o      => pv_Unidad_o
                                   ,pv_nmordina_o    => n_nmordina
                                   ,pn_cdRemanente_i => NULL
                                   ,pv_CdTipo_i      => pv_CdTipo_i);
    END IF;
    pv_Unidad_o := pv_UnidadC;
    n_Return    := n_Contratado - n_Pagado - n_Transito - n_Dictamen;
    RETURN n_Return;
  END;

  PROCEDURE P_TVALOREMADET_DEL(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                              ,pv_status_i   IN TVALOREMADET.status%TYPE) IS
  BEGIN
    BEGIN
      DELETE tvaloremadet tva
       WHERE tva.ntramite = pn_ntramite_i
         AND tva.status = pv_status_i;
    END;
  END;

  FUNCTION f_unidad_get(pn_idremanente_i IN tvalorema.idremanente%TYPE)
    RETURN VARCHAR2 IS
    CURSOR Cur_Rem IS
      SELECT otvalor02
        FROM tvaloremaDET
       WHERE idremanente = pn_idremanente_i
         AND NMORDINA = (SELECT MAX(NMORDINA)
                           FROM TVALOREMADET D
                          WHERE D.IDREMANENTE = pn_idremanente_i
                            AND d.status = 'R');
    v_Return VARCHAR2(10);
  BEGIN
    IF pn_idremanente_i IS NOT NULL THEN
      OPEN cur_rem;
      FETCH cur_rem
        INTO v_Return;
      CLOSE cur_rem;
    ELSE
      v_Return := NULL;
    END IF;
    RETURN v_Return;
  END;

  FUNCTION F_ESTABLECER_CONTADORES(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN VARCHAR2 IS
    n_IdRemanente PLS_INTEGER;
    n_NmOrdina    PLS_INTEGER;
    v_cdconval    VARCHAR2(10);
    v_Unidad_TA   VARCHAR2(20);
    CURSOR Cur_Rem(pv_CdConVal_i VARCHAR2) IS
      SELECT t.cdremanente
            ,t.cdtiporema
            ,t.cdnivel
            ,t.cdconval
        FROM tconfrema t
       WHERE cdconval = pv_CdConVal_i
         AND cdramo = aVariables('CDRAMO')
         AND T.SWACTIVO = K_S;
    Reg_Rem  Cur_Rem%ROWTYPE;
    aValores t_100_30;
    --
    CURSOR Cur_Gar IS
      SELECT t.CDGARANT
            ,t.cdtipo
            ,t.cdramo
        FROM TSINIGAR T
       WHERE T.NTRAMITE = pn_ntramite_i
       ORDER BY T.CDGARANT;
    Reg_Gar Cur_Gar%ROWTYPE;
    --
    CURSOR Cur_Cont IS
      SELECT CC.CONCEPTO_REMANENTE
            ,CC.UNIDAD
        FROM vw_clasprog_cont CC
        JOIN TRAGACOV T
          ON T.CDCONVAL = CC.concepto_remanente
         AND T.CDTIPVAL = CC.CDTIPVAL
       WHERE T.CDRAMO = aVariables('CDRAMO')
      UNION
      SELECT CC.CONCEPTO_REMANENTE
            ,CC.UNIDAD
        FROM vw_concgast_cont CC
        JOIN TRAGACOV T
          ON T.CDCONVAL = CC.concepto_remanente
         AND T.CDTIPVAL = CC.CDTIPVAL
       WHERE T.CDRAMO = aVariables('CDRAMO')
      UNION
      SELECT CC.CDCONVAL
            ,CC.UNIDAD
        FROM siniestros.VW_INDEMNIZA_CONT CC
        JOIN TRAGACOV T
          ON T.CDCONVAL = CC.CDCONVAL
         AND T.CDRAMO = CC.CDRAMO
         AND T.CDGARANT = CC.CDGARANT
       WHERE T.CDRAMO = aVariables('CDRAMO');
    --
    PROCEDURE P_VALOR_SET(pv_otvalor_i IN VARCHAR2) IS
    BEGIN
      aValores(pv_otvalor_i) := aRemanente(Reg_Rem.Cdremanente)
                                (pv_otvalor_i);
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        aValores(pv_otvalor_i) := NULL;
    END;
  
    PROCEDURE P_REMA_DET_UPD(pv_Status_i IN VARCHAR2) IS
    BEGIN
      --Seteamos los valores
      P_VALOR_SET('OTVALOR01');
      P_VALOR_SET('OTVALOR02');
      P_VALOR_SET('OTVALOR03');
      P_VALOR_SET('OTVALOR04');
      P_VALOR_SET('OTVALOR05');
      --Insertamos el detalle
      IF aValores('OTVALOR01') IS NOT NULL THEN
        P_TVALOREMADET_UPD(pn_IdRemanente_i => n_IdRemanente
                          ,pn_NmOrdina_i    => n_NmOrdina
                          ,pv_Status_i      => pv_Status_i
                          ,pd_FESTATUS_i    => SYSDATE
                          ,pv_OtValor01_i   => aValores('OTVALOR01')
                          ,pv_OtValor02_i   => aValores('OTVALOR02')
                          ,pv_OtValor03_i   => aValores('OTVALOR03')
                          ,pv_OtValor04_i   => aValores('OTVALOR04')
                          ,pv_OtValor05_i   => aValores('OTVALOR05'));
      END IF;
    END;
  
    PROCEDURE P_REMA_DET_INS(pv_Status_i IN VARCHAR2) IS
    BEGIN
      --Seteamos los valores
      P_VALOR_SET('OTVALOR01');
      P_VALOR_SET('OTVALOR02');
      P_VALOR_SET('OTVALOR03');
      P_VALOR_SET('OTVALOR04');
      P_VALOR_SET('OTVALOR05');
      --Insertamos el detalle
      IF aValores('OTVALOR01') IS NOT NULL THEN
        P_TVALOREMADET_INS(pn_IdRemanente_i => n_IdRemanente
                          ,pn_NmOrdina_i    => n_NmOrdina
                          ,pv_CdConval_i    => v_cdconval
                          ,pv_Status_i      => pv_Status_i
                          ,pd_FESTATUS_i    => SYSDATE
                          ,pv_OtValor01_i   => aValores('OTVALOR01')
                          ,pv_OtValor02_i   => aValores('OTVALOR02')
                          ,pv_OtValor03_i   => aValores('OTVALOR03')
                          ,pv_OtValor04_i   => aValores('OTVALOR04')
                          ,pv_OtValor05_i   => aValores('OTVALOR05'));
      END IF;
    END;
  
    PROCEDURE P_NUEVO_REMANENTE IS
    BEGIN
      n_IdRemanente := F_TVALOREMA_INS(pv_CdNivel_i     => Reg_Rem.Cdnivel
                                      ,pn_CdRemanente_i => Reg_Rem.Cdremanente
                                      ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
      --Numero ordinal 0 para el registro
      n_NmOrdina := 0;
      --Leemos el contratado
      LOAD_CONTRATADO(pn_CdRemanente_i => Reg_Rem.Cdremanente
                     ,pv_CdTipoRema_i  => Reg_Rem.Cdtiporema
                     ,pv_CdNivel_i     => Reg_Rem.Cdnivel
                     ,pv_CdConval_i    => Reg_Rem.cdconval
                     ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo
                     ,pv_UnidadCont_i  => v_Unidad_TA);
      P_REMA_DET_INS('R');
    END;
  
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    OPEN Cur_Cont;
    LOOP
      FETCH cur_cont
        INTO v_cdconval
            ,v_Unidad_TA;
      EXIT WHEN cur_cont%NOTFOUND;
      OPEN Cur_Rem(v_cdconval);
      LOOP
        FETCH cur_rem
          INTO reg_rem;
        EXIT WHEN cur_rem%NOTFOUND;
        --Validamos si existe remanente
        n_IdRemanente := F_TVALOREMA_GET(pv_CdNivel_i     => Reg_Rem.Cdnivel
                                        ,pn_CdRemanente_i => Reg_Rem.Cdremanente
                                        ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
        IF n_IdRemanente IS NULL THEN
          --Si no existe
          --Insertamos el remanente
          P_NUEVO_REMANENTE;
        END IF;
      END LOOP;
      CLOSE cur_rem;
    END LOOP;
    CLOSE cur_cont;
    RETURN 'ORDS-1003';
  END;

  PROCEDURE P_REGISTRAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
    n_IdRemanente PLS_INTEGER;
    n_NmOrdina    PLS_INTEGER;
    v_cdconval    VARCHAR2(10);
    CURSOR Cur_Rem(pv_CdConVal_i VARCHAR2) IS
      SELECT t.cdremanente
            ,t.cdtiporema
            ,t.cdnivel
            ,t.cdconval
            ,T.OWNIVEL
            ,T.TANIVEL
            ,T.CANIVEL
        FROM tconfrema t
       WHERE (cdconval = pv_CdConVal_i OR
             (pv_CdConVal_i IS NULL AND t.cdtiporema = 'CO'))
         AND cdramo = aVariables('CDRAMO')
         AND SWACTIVO = K_S;
    Reg_Rem  Cur_Rem%ROWTYPE;
    aValores t_100_30;
    n_Suma   NUMBER;
    --
    CURSOR Cur_Gar IS
      SELECT t.CDGARANT
            ,t.cdtipo
        FROM TSINIGAR T
       WHERE T.NTRAMITE = pn_ntramite_i
       ORDER BY T.CDGARANT;
    Reg_Gar Cur_Gar%ROWTYPE;
    --
    CURSOR Cur_Tvv IS
      SELECT t.cdtipval
            ,t.cdconval
            ,t.cdconcep
        FROM TVALOVAL t
       WHERE NTRAMITE = pn_ntramite_i
         AND CDGARANT = Reg_Gar.CdGarant
       ORDER BY T.CDCONVAL;
    Reg_Tvv Cur_Tvv%ROWTYPE;
    --
    v_UnidadCont VARCHAR2(100);
    --
    v_Nivel_Dinamico VARCHAR2(3);
    --
    FUNCTION F_EXISTE_TVALOVAL RETURN BOOLEAN IS
      CURSOR Cur_TVALOVAL IS
        SELECT ROWNUM FROM TVALOVAL WHERE NTRAMITE = pn_ntramite_i;
      b_Return BOOLEAN;
      n_Dummy  PLS_INTEGER;
    BEGIN
      OPEN cur_Tvaloval;
      FETCH cur_tvaloval
        INTO n_Dummy;
      b_Return := Cur_TVALOVAL%FOUND;
      CLOSE cur_tvaloval;
      RETURN b_Return;
    END;
  
    FUNCTION F_NIVEL_GET(pn_CdRemanente_i IN TCONFREMA.CDREMANENTE%TYPE
                        ,pv_CdTipoRema_i  IN TCONFREMA.Cdtiporema%TYPE
                        ,pv_CdNivel_i     IN TCONFREMA.CdNivel%TYPE
                        ,pv_CdConval_i    IN TCONFREMA.cdconval%TYPE)
      RETURN VARCHAR2 IS
      CURSOR Cur_Ctto IS
        SELECT T.OWNIVEL
              ,T.TANIVEL
              ,T.CANIVEL
          FROM tconfrema T
         WHERE t.cdramo = aVariables('CDRAMO')
           AND T.CDREMANENTE = pn_CdRemanente_i
           AND T.CDTIPOREMA = pv_CdTipoRema_i
           AND t.cdnivel = pv_CdNivel_i
           AND t.cdconval = pv_CdConval_i
           AND T.SWACTIVO = K_S;
      Reg_Ctto Cur_Ctto%ROWTYPE;
      v_Nivel  VARCHAR2(3);
      CURSOR Cur_Nivel IS
        SELECT OTVALOR02
          FROM TTAPVAAT
         WHERE NMTABLA IN
               (SELECT NMTABLA FROM TTAPTABL WHERE CDTABLA = 'TNIVELES')
           AND otclave1 = v_Nivel;
      --
      FUNCTION F_VALOR(pv_Tabla_i IN VARCHAR2
                      ,pv_Campo_i IN VARCHAR2) RETURN VARCHAR2 IS
        v_Return VARCHAR2(100);
      BEGIN
        IF pv_campo_i IS NOT NULL THEN
          CASE pv_Tabla_i
            WHEN 'TVALOSIT' THEN
              v_Return := aTvalosit(pv_campo_i);
            WHEN 'TVALOGAR' THEN
              v_Return := aTvalogar(pv_campo_i);
            ELSE
              v_return := NULL;
          END CASE;
        ELSE
          v_return := NULL;
        END IF;
        RETURN v_return;
      END;
    
    BEGIN
      OPEN CUR_CTTO;
      FETCH CUR_CTTO
        INTO REG_CTTO;
      CLOSE CUR_CTTO;
      v_Nivel := F_VALOR(Reg_Ctto.Tanivel
                        ,Reg_Ctto.Canivel);
      OPEN Cur_Nivel;
      FETCH Cur_Nivel
        INTO v_Nivel;
      CLOSE cur_nivel;
      RETURN v_nivel;
    END;
  
    --
    PROCEDURE LOAD_GASTADO(pn_CdRemanente_i IN TCONFREMA.CDREMANENTE%TYPE
                          ,pv_CdConcepto_i  IN VARCHAR2
                          ,pv_CdTipVal_i    IN VARCHAR2
                          ,pv_CdTipoRema_i  IN TCONFREMA.Cdtiporema%TYPE
                          ,pv_CdNivel_i     IN TCONFREMA.CdNivel%TYPE
                          ,pv_CdConval_i    IN TCONFREMA.cdconval%TYPE
                          ,pv_CdTipoGar_i   IN VARCHAR2 DEFAULT 'P') IS
      CURSOR Cur_Ctto IS
        SELECT T.OWAFECTA
              ,T.TAAFECTA
              ,T.CAAFECTA
              ,T.OWUNAFECTA
              ,T.TAUNAFECTA
              ,T.UNAFECTA
              ,T.OWREMANE
              ,T.TAREMANE
              ,T.CAREMANE
              ,T.OWUNREMANE
              ,T.TAUNREMANE
              ,T.UNREMANE
          FROM tconfrema T
         WHERE t.cdramo = aVariables('CDRAMO')
           AND T.CDREMANENTE = pn_CdRemanente_i
           AND T.CDTIPOREMA = pv_CdTipoRema_i
           AND t.cdnivel = pv_CdNivel_i
           AND t.cdconval = pv_CdConval_i
           AND t.swactivo = K_S;
      Reg_Ctto       Cur_Ctto%ROWTYPE;
      v_CdGarant     VARCHAR2(4);
      d_FeOcurrencia DATE := pkg_reglas_cal.f_ocurrencia_tramite_get(pn_ntramite_i => aMesaControl(1).Ntramite);
      n_Tasa         NUMBER;
      --
      FUNCTION F_VALOR(pv_Tabla_i IN VARCHAR2
                      ,pv_Campo_i IN VARCHAR2) RETURN VARCHAR2 IS
        v_Return VARCHAR2(100);
      BEGIN
        IF pv_Campo_i IS NOT NULL THEN
          BEGIN
            CASE pv_Tabla_i
              WHEN 'TVALOVAL' THEN
                v_Return := aTvaloVal(v_CdGarant) (pv_CdTipVal_i)
                            (pv_CdConcepto_i) (pv_campo_i);
              ELSE
                v_return := NULL;
            END CASE;
          EXCEPTION
            WHEN NO_dATA_FOUND THEN
              v_return := NULL;
          END;
        ELSE
          v_return := NULL;
        END IF;
        RETURN v_return;
      END;
    
    BEGIN
      IF pv_CdTipoGar_i = 'P' THEN
        v_CdGarant := aCobertura(1).vCdGarant;
      ELSE
        v_CdGarant := aCoberturaAux(1).vCdGarant;
      END IF;
      OPEN CUR_CTTO;
      LOOP
        FETCH CUR_CTTO
          INTO REG_CTTO;
        EXIT WHEN cur_ctto%NOTFOUND;
        aRemanente(pn_CdRemanente_i)(REG_CTTO.CAREMANE) := F_VALOR(Reg_Ctto.TAAFECTA
                                                                  ,Reg_Ctto.Caafecta);
        aRemanente(pn_CdRemanente_i)(REG_CTTO.UNREMANE) := F_VALOR(Reg_Ctto.Taunafecta
                                                                  ,Reg_Ctto.Unafecta);
        IF aRemanente(pn_CdRemanente_i) (REG_CTTO.UNREMANE) IS NULL THEN
          aRemanente(pn_CdRemanente_i)(REG_CTTO.UNREMANE) := v_UnidadCont;
        END IF;
        IF aRemanente(pn_CdRemanente_i) (REG_CTTO.UNREMANE) != v_UnidadCont THEN
          IF aRemanente(pn_CdRemanente_i) (REG_CTTO.UNREMANE) != 'MXN' THEN
            n_Tasa := F_TTIPCAMB_GET(aRemanente(pn_CdRemanente_i)
                                     (REG_CTTO.UNREMANE)
                                    ,trunc(d_FeOcurrencia));
            aRemanente(pn_CdRemanente_i)(REG_CTTO.CAREMANE) := aRemanente(pn_CdRemanente_i)
                                                               (REG_CTTO.CAREMANE) *
                                                               n_Tasa;
          END IF;
          IF v_UnidadCont != 'MXN' THEN
            n_Tasa := F_TTIPCAMB_GET(v_UnidadCont
                                    ,trunc(d_FeOcurrencia));
            aRemanente(pn_CdRemanente_i)(REG_CTTO.CAREMANE) := aRemanente(pn_CdRemanente_i)
                                                               (REG_CTTO.CAREMANE) /
                                                               n_tasa;
          END IF;
          aRemanente(pn_CdRemanente_i)(REG_CTTO.UNREMANE) := v_UnidadCont;
        END IF;
      END LOOP;
      CLOSE CUR_CTTO;
    END;
  
    PROCEDURE P_VALOR_SET(pv_otvalor_i IN VARCHAR2) IS
    BEGIN
      aValores(pv_otvalor_i) := aRemanente(Reg_Rem.Cdremanente)
                                (pv_otvalor_i);
      dbms_output.put_line(pv_otvalor_i || ':' || aValores(pv_otvalor_i));
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        aValores(pv_otvalor_i) := NULL;
    END;
  
    PROCEDURE P_REMA_DET_UPD(pv_Status_i IN VARCHAR2) IS
    BEGIN
      --Seteamos los valores
      P_VALOR_SET('OTVALOR01');
      P_VALOR_SET('OTVALOR02');
      P_VALOR_SET('OTVALOR03');
      P_VALOR_SET('OTVALOR04');
      P_VALOR_SET('OTVALOR05');
      --Insertamos el detalle
      IF aValores('OTVALOR01') IS NOT NULL THEN
        P_TVALOREMADET_UPD(pn_IdRemanente_i => n_IdRemanente
                          ,pn_NmOrdina_i    => n_NmOrdina
                          ,pv_Status_i      => pv_Status_i
                          ,pd_FESTATUS_i    => SYSDATE
                          ,pv_OtValor01_i   => round(aValores('OTVALOR01')
                                                    ,2)
                          ,pv_OtValor02_i   => aValores('OTVALOR02')
                          ,pv_OtValor03_i   => aValores('OTVALOR03')
                          ,pv_OtValor04_i   => aValores('OTVALOR04')
                          ,pv_OtValor05_i   => aValores('OTVALOR05'));
      END IF;
    END;
  
    PROCEDURE P_REMA_DET_INS(pv_Status_i IN VARCHAR2) IS
    BEGIN
      --Seteamos los valores
      P_VALOR_SET('OTVALOR01');
      P_VALOR_SET('OTVALOR02');
      P_VALOR_SET('OTVALOR03');
      P_VALOR_SET('OTVALOR04');
      P_VALOR_SET('OTVALOR05');
      --Insertamos el detalle
      IF aValores('OTVALOR01') IS NOT NULL THEN
        P_TVALOREMADET_INS(pn_IdRemanente_i => n_IdRemanente
                          ,pn_NmOrdina_i    => n_NmOrdina
                          ,pv_CdConval_i    => v_cdconval
                          ,pv_Status_i      => pv_Status_i
                          ,pd_FESTATUS_i    => SYSDATE
                          ,pv_OtValor01_i   => round(aValores('OTVALOR01')
                                                    ,2)
                          ,pv_OtValor02_i   => aValores('OTVALOR02')
                          ,pv_OtValor03_i   => aValores('OTVALOR03')
                          ,pv_OtValor04_i   => aValores('OTVALOR04')
                          ,pv_OtValor05_i   => aValores('OTVALOR05'));
      END IF;
    END;
  
    PROCEDURE P_NUEVO_REMANENTE IS
    BEGIN
      n_IdRemanente := F_TVALOREMA_INS(pv_CdNivel_i     => Reg_Rem.Cdnivel
                                      ,pn_CdRemanente_i => Reg_Rem.Cdremanente
                                      ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
      --Numero ordinal 0 para el registro
      n_NmOrdina := 0;
      --Leemos el contratado
      LOAD_CONTRATADO(pn_CdRemanente_i => Reg_Rem.Cdremanente
                     ,pv_CdTipoRema_i  => Reg_Rem.Cdtiporema
                     ,pv_CdNivel_i     => Reg_Rem.Cdnivel
                     ,pv_CdConval_i    => Reg_Rem.cdconval
                     ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
      P_REMA_DET_INS('R');
    END;
  
    PROCEDURE P_GASTADO IS
      b_Existe_Tvalorema_Det BOOLEAN;
    BEGIN
      --REMANENTE EXISTENTE
      aRemanente.Delete();
      --Buscamos el maximo numero de ordinal mas 1      
      n_NmOrdina := F_MAX_NUMORD(pn_IdRemanente_i => n_IdRemanente
                                 --,pv_CdConval_i   => v_cdconval
                                 );
      --Cargamos lo Afectado
      LOAD_GASTADO(pn_CdRemanente_i => Reg_Rem.Cdremanente
                  ,pv_CdConcepto_i  => reg_tvv.cdconcep
                  ,pv_CdTipVal_i    => reg_tvv.cdtipval
                  ,pv_CdTipoRema_i  => Reg_Rem.Cdtiporema
                  ,pv_CdNivel_i     => Reg_Rem.Cdnivel
                  ,pv_CdConval_i    => Reg_Rem.cdconval
                  ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
      --
      b_Existe_Tvalorema_Det := F_TVALOREMADET_EXISTE_GET(pn_IdRemanente_i => n_IdRemanente
                                                         ,pn_nmordina_i    => n_NmOrdina
                                                         ,pn_ntramite_i    => pn_ntramite_i
                                                         ,pv_CdConval_i    => v_cdconval
                                                         ,pv_status_i      => 'D');
      IF b_Existe_Tvalorema_Det THEN
        P_REMA_DET_UPD('D');
      ELSE
        n_NmOrdina := n_NmOrdina + 1;
        P_REMA_DET_INS('D');
      END IF;
    END;
  
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    P_TVALOREMADET_DEL(pn_ntramite_i
                      ,'D');
    OPEN Cur_Gar;
    LOOP
      FETCH Cur_Gar
        INTO Reg_Gar;
      EXIT WHEN Cur_Gar%NOTFOUND;
      IF F_EXISTE_TVALOVAL THEN
        OPEN Cur_Tvv;
        LOOP
          FETCH cur_tvv
            INTO reg_tvv;
          EXIT WHEN cur_tvv%NOTFOUND;
          v_cdconval := reg_tvv.cdconval;
          -- dbms_output.put_line('v_cdconval: '||v_cdconval);
          OPEN Cur_Rem(v_cdconval);
          LOOP
            FETCH cur_rem
              INTO reg_rem;
            EXIT WHEN cur_rem%NOTFOUND;
            v_Nivel_Dinamico := F_NIVEL_GET(pn_CdRemanente_i => Reg_Rem.Cdremanente
                                           ,pv_CdTipoRema_i  => Reg_Rem.Cdtiporema
                                           ,pv_CdNivel_i     => Reg_Rem.Cdnivel
                                           ,pv_CdConval_i    => Reg_Rem.cdconval);
            IF v_Nivel_Dinamico IS NOT NULL THEN
              Reg_Rem.Cdnivel := v_Nivel_Dinamico;
            END IF;
            --Validamos si existe remanente
            --          dbms_output.put_line(Reg_Rem.Cdremanente);
            n_IdRemanente := F_TVALOREMA_GET(pv_CdNivel_i     => Reg_Rem.Cdnivel
                                            ,pn_CdRemanente_i => Reg_Rem.Cdremanente
                                            ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
            IF n_IdRemanente IS NULL THEN
              --Si no existe
              --Insertamos el remanente
              P_NUEVO_REMANENTE;
              --                COMMIT;
              --Insertamos el Gastado
              --v_UnidadCont := aValores('OTVALOR02'); --f_unidad_get(pn_idremanente_i => n_idremanente);
              v_UnidadCont := f_unidad_get(pn_idremanente_i => n_idremanente);
              -- if v_UnidadCont != aRemanente()
              P_GASTADO;
              --                COMMIT;
              --Sumamos los Registrados - los pagados
              n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                           ,pv_Status_i      => 'R') -
                        F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                           ,pv_Status_i      => 'P');
              --Actualizamos el Remanente con la suma del detalle
              P_TVALOREMA_UPD(pn_IdRemanente_i => n_IdRemanente
                             ,pv_OtValor01_i   => n_Suma
                             ,pv_OtValor02_i   => v_UnidadCont);
            ELSE
              v_unidadCont := f_unidad_get(n_IdRemanente);
              P_GASTADO;
              --                COMMIT;
              --
              --Sumamos los Registrados - los pagados
              n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                           ,pv_Status_i      => 'R') -
                        F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                           ,pv_Status_i      => 'P');
              --Actualizamos el Remanente con la suma del detalle
              P_TVALOREMA_UPD(pn_IdRemanente_i => n_IdRemanente
                             ,pv_OtValor01_i   => n_suma
                             ,pv_OtValor02_i   => aValores('OTVALOR02'));
            END IF;
          END LOOP;
          CLOSE cur_rem;
        END LOOP;
        CLOSE cur_tvv;
      ELSE
        OPEN Cur_Rem(v_cdconval);
        LOOP
          FETCH cur_rem
            INTO reg_rem;
          EXIT WHEN cur_rem%NOTFOUND;
          v_cdconval       := Reg_Rem.cdconval;
          v_Nivel_Dinamico := F_NIVEL_GET(pn_CdRemanente_i => Reg_Rem.Cdremanente
                                         ,pv_CdTipoRema_i  => Reg_Rem.Cdtiporema
                                         ,pv_CdNivel_i     => Reg_Rem.Cdnivel
                                         ,pv_CdConval_i    => Reg_Rem.cdconval);
          IF v_Nivel_Dinamico IS NOT NULL THEN
            Reg_Rem.Cdnivel := v_Nivel_Dinamico;
          END IF;
          --Validamos si existe remanente
          --          dbms_output.put_line(Reg_Rem.Cdremanente);
          n_IdRemanente := F_TVALOREMA_GET(pv_CdNivel_i     => Reg_Rem.Cdnivel
                                          ,pn_CdRemanente_i => Reg_Rem.Cdremanente
                                          ,pv_CdTipoGar_i   => Reg_Gar.Cdtipo);
          IF n_IdRemanente IS NULL THEN
            --Si no existe
            --Insertamos el remanente
            P_NUEVO_REMANENTE;
            --                COMMIT;
            --Insertamos el Gastado
            --v_UnidadCont := aValores('OTVALOR02'); --f_unidad_get(pn_idremanente_i => n_idremanente);
            v_UnidadCont := f_unidad_get(pn_idremanente_i => n_idremanente);
            -- if v_UnidadCont != aRemanente()
            --Sumamos los Registrados - los pagados
            n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                         ,pv_Status_i      => 'R') -
                      F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                         ,pv_Status_i      => 'P');
            --Actualizamos el Remanente con la suma del detalle
            P_TVALOREMA_UPD(pn_IdRemanente_i => n_IdRemanente
                           ,pv_OtValor01_i   => n_Suma
                           ,pv_OtValor02_i   => v_UnidadCont);
            /*ELSE
            v_unidadCont := f_unidad_get(n_IdRemanente);
            P_GASTADO;
            --                COMMIT;
            --
            --Sumamos los Registrados - los pagados
            n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                         ,pv_Status_i      => 'R') -
                      F_SUM_OTVALOR01_GET(pn_IdRemanente_i => n_IdRemanente
                                         ,pv_Status_i      => 'P');
            --Actualizamos el Remanente con la suma del detalle
            P_TVALOREMA_UPD(pn_IdRemanente_i => n_IdRemanente
                           ,pv_OtValor01_i   => n_suma
                           ,pv_OtValor02_i   => aValores('OTVALOR02'));*/
          END IF;
        END LOOP;
        CLOSE cur_rem;
      END IF;
    END LOOP;
    CLOSE CUR_GAR;
  END;

  PROCEDURE p_tdetimptra(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
    -- --
    v_sql         VARCHAR2(4000);
    n_existe      NUMBER;
    v_caafecta    VARCHAR2(200);
    pv_userregi_i VARCHAR2(200) := USER;
    pd_feregist_i DATE := SYSDATE;
    -- --
    CURSOR cur_datos IS
      SELECT 'OTVALOR' || CASE
               WHEN tat.cdatribu < 100 THEN
                TRIM(to_char(tat.cdatribu
                            ,'00'))
               ELSE
                TRIM(to_char(tat.cdatribu))
             END AS cdatribu
            ,tsi.cdgarant
            ,tra.cdtipval
            ,tat.idatribu
            ,tco.cdconval
            ,tco.dsconval
            ,tv.otvalor01
            ,tv.otvalor02
        FROM tatrival tat
            ,tragacov tra
            ,tsinigar tsi
            ,tconcsin tco
            ,ttapvaat tv
       WHERE tat.cdramo = tra.cdramo
         AND tat.cdramo = tsi.cdramo
         AND tat.cdtipval = tra.cdtipval
         AND tsi.cdgarant = tra.cdgarant
         AND tsi.ntramite = pn_ntramite_i
         AND tv.nmtabla = (SELECT tt.nmtabla
                             FROM ttaptabl tt
                            WHERE tt.cdtabla = 'DETRAMIVAL')
         AND tv.otclave1 = aVTMesaControl('MODALIDAD DEL TRAMITE')
         AND tv.otclave2 = tco.cdconval
         AND tv.otclave3 = tat.idatribu
         AND tco.cdtipcon IS NOT NULL
         AND tco.cdconval = tra.cdconval
         AND tat.idatribu IN ('UNIDADREMANENTE'
                             ,'UNIDAD'
                             ,'MONEDAIMPORTEFACTURA'
                             ,'IMPORTEFACTURA'
                             ,'IMPORTE'
                             ,'CANTUNIDAPROBADAS');
    -- --  
    reg_datos cur_datos%ROWTYPE;
    -- --
  BEGIN
    -- --
    load_datos(pn_ntramite_i);
    -- p_tvaloremadet_del(pn_ntramite_i,'D');
    -- --
    OPEN cur_datos;
    LOOP
      FETCH cur_datos
        INTO reg_datos;
      EXIT WHEN cur_datos%NOTFOUND;
      -- --
      IF reg_datos.otvalor02 = 'IMPORTE' THEN
        v_caafecta := to_char(pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                                 ,reg_datos.otvalor01));
      ELSIF reg_datos.otvalor02 = 'UNIDAD' THEN
        v_caafecta := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                         ,reg_datos.otvalor01);
      END IF;
      -- --
      BEGIN
        v_sql := ' select 1 from  TVALOVAL xx  
                    where xx.ntramite  = :1 
                      and xx.cdunieco  = :2
                      and xx.cdramo    = :3
                      and xx.aaapertu  = :4
                      and xx.status    = :5
                      and xx.nmsinies  = :6
                      and xx.cdgarant  = :7
                      and xx.cdconcep  = :8
                      and xx.cdconval  = :9
                      and xx.cdtipval  = :10';
        --        dbms_output.put_line('1' || v_sql);
        EXECUTE IMMEDIATE v_sql
          INTO n_existe
          USING amesacontrol(1).ntramite, amesacontrol(1).cdunieco, amesacontrol(1).cdramo, asiniestro(1).aaapertu, asiniestro(1).status, asiniestro(1).nmsinies, reg_datos.cdgarant, '?', reg_datos.cdconval, reg_datos.cdtipval;
      EXCEPTION
        WHEN no_data_found THEN
          n_existe := 0;
      END;
      -- --
      IF nvl(n_existe
            ,0) = 1 THEN
        v_sql := ' update TVALOVAL xx ';
        v_sql := v_sql || 'set xx.' || reg_datos.cdatribu || '=' || chr(39) ||
                 v_caafecta || chr(39);
        v_sql := v_sql || ' where xx.ntramite  = :1';
        v_sql := v_sql || ' and xx.cdunieco    = :2';
        v_sql := v_sql || ' and xx.cdramo      = :3';
        v_sql := v_sql || ' and xx.aaapertu    = :4';
        v_sql := v_sql || ' and xx.status      = :5';
        v_sql := v_sql || ' and xx.nmsinies    = :6';
        v_sql := v_sql || ' and xx.cdgarant    = :7';
        v_sql := v_sql || ' and xx.cdconcep    = :8';
        v_sql := v_sql || ' and xx.cdconval    = :9';
        v_sql := v_sql || ' and xx.cdtipval    = :10';
        --        dbms_output.put_line('2' || v_sql);
        EXECUTE IMMEDIATE v_sql
          USING amesacontrol(1).ntramite, amesacontrol(1).cdunieco, amesacontrol(1).cdramo, asiniestro(1).aaapertu, asiniestro(1).status, asiniestro(1).nmsinies, reg_datos.cdgarant, '?', reg_datos.cdconval, reg_datos.cdtipval;
        -- --
      ELSIF nvl(n_existe
               ,0) = 0 THEN
        -- --
        v_sql := 'insert into TVALOVAL';
        v_sql := v_sql ||
                 '(ntramite, cdunieco, cdramo, aaapertu, status, nmsinies
                          , cdgarant, cdtipval, cdconval, cdconcep, userregi, feregist, ' ||
                 reg_datos.cdatribu || ')';
        v_sql := v_sql ||
                 'values (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13)';
        --        dbms_output.put_line('3' || v_sql);
        EXECUTE IMMEDIATE v_sql
          USING amesacontrol(1).ntramite, amesacontrol(1).cdunieco, amesacontrol(1).cdramo, asiniestro(1).aaapertu, asiniestro(1).status, asiniestro(1).nmsinies, reg_datos.cdgarant, reg_datos.cdtipval, reg_datos.cdconval, '?', pv_userregi_i, pd_feregist_i, v_caafecta;
        -- --
      END IF;
    END LOOP;
    CLOSE cur_datos;
  END p_tdetimptra;

  FUNCTION F_VTVALOSIT_GET(pv_IdAtributo_i IN VARCHAR2) RETURN VARCHAR2 IS
    CURSOR Cur_Vtvalosit IS
      SELECT mps.otvalor
        FROM VTVALOSIT MPS
       WHERE MPS.CDUNIECO = aPoliza(1).Cdunieco
         AND mpS.cdramo = aPoliza(1).CdRamo
         AND mpS.estado = aPoliza(1).Estado
         AND mpS.nmpoliza = aPoliza(1).NmPoliza
         AND MPS.NMSITUAC = aSituacion(1).NmSituac
         AND MPS.idatribu = pv_IdAtributo_i
         AND mps.nmsuplem =
             (SELECT MAX(NMSUPLEM)
                FROM MPOLISIT MPS2
               WHERE MPS2.CDUNIECO = MPS.cdunieco
                 AND MPS2.cdramo = MPS.cdramo
                 AND MPS2.estado = MPS.estado
                 AND MPS2.nmpoliza = MPS.nmpoliza
                 AND MPS2.NMSITUAC = MPS.NMSITUAC
                 AND MPS2.nmsuplem <= aSiniestro(1).Nmsuplem);
    v_OtValor VARCHAR2(200);
  BEGIN
    OPEN Cur_Vtvalosit;
    FETCH Cur_Vtvalosit
      INTO v_OtValor;
    CLOSE Cur_Vtvalosit;
    RETURN v_OtValor;
  END;

  FUNCTION F_VTVALOGAR_GET(pv_IdAtributo_i IN VARCHAR2) RETURN VARCHAR2 IS
    CURSOR Cur_Vtvalosit IS
      SELECT mps.otvalor
        FROM VTVALOGAR MPS
       WHERE MPS.CDUNIECO = aPoliza(1).Cdunieco
         AND mpS.cdramo = aPoliza(1).CdRamo
         AND mpS.estado = aPoliza(1).Estado
         AND mpS.nmpoliza = aPoliza(1).NmPoliza
         AND MPS.NMSITUAC = aSituacion(1).NmSituac
         AND MPS.cdgarant = aCobertura(1).vCdGarant
         AND MPS.idatribu = pv_IdAtributo_i
         AND mps.nmsuplem =
             (SELECT MAX(NMSUPLEM)
                FROM TVALOGAR MPS2
               WHERE MPS2.CDUNIECO = MPS.cdunieco
                 AND MPS2.cdramo = MPS.cdramo
                 AND MPS2.estado = MPS.estado
                 AND MPS2.nmpoliza = MPS.nmpoliza
                 AND MPS2.NMSITUAC = MPS.NMSITUAC
                 AND MPS2.CDGARANT = MPS.cdgarant
                 AND MPS2.nmsuplem <= aSiniestro(1).Nmsuplem);
    v_OtValor VARCHAR2(200);
  BEGIN
    OPEN Cur_Vtvalosit;
    FETCH Cur_Vtvalosit
      INTO v_OtValor;
    CLOSE Cur_Vtvalosit;
    RETURN v_OtValor;
  END;

  FUNCTION F_ACUMULADOS_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN SYS_REFCURSOR IS
    aAcumulados         t_tab_acumulados;
    i                   PLS_INTEGER := 0;
    v_DsConcepto        VARCHAR2(100);
    n_ImporteC          NUMBER;
    n_ImporteT          NUMBER;
    n_ImporteR          NUMBER;
    v_Unidad            VARCHAR2(15);
    v_UnidadC           VARCHAR2(15);
    v_Dummy             VARCHAR2(15);
    psrc_Data_o         SYS_REFCURSOR;
    n_ImporteP          NUMBER;
    v_Encabezado        VARCHAR2(200);
    d_FeOcurrencia      DATE := PKG_REGLAS_CAL.F_OCURRENCIA_TRAMITE_GET(pn_ntramite_i => pn_ntramite_i);
    n_ImporteConvertido NUMBER;
    n_ImporteExceso     NUMBER := 0;
    n_nmordina          NUMBER;
    --
    PROCEDURE P_CONCEPTO_SET(pv_TipoRema_i   IN VARCHAR2
                            ,pv_CdNivel_i    IN VARCHAR2
                            ,pv_Status_i     IN VARCHAR2 DEFAULT 'P'
                            ,pv_PorTramite_i IN VARCHAR2 DEFAULT 'N'
                            ,pv_CdTipCon_i   IN VARCHAR2
                            ,pv_Tipo_i       IN VARCHAR2) IS
      FUNCTION F_TCONCSIN_DESC_GET(pv_cdtipcon_i IN TCONCSIN.cdtipcon%TYPE)
        RETURN VARCHAR2 IS
        CURSOR Cur_DsCpto IS
          SELECT t.dsconval
            FROM TCONCSIN t
           WHERE t.cdtipcon = pv_cdtipcon_i;
        v_DsConcepto TCONCSIN.DSCONVAL%TYPE;
      BEGIN
        OPEN Cur_DsCpto;
        FETCH Cur_DsCpto
          INTO v_DsConcepto;
        CLOSE Cur_DsCpto;
        RETURN v_DsConcepto;
      END;
    
    BEGIN
      IF pv_CdTipCon_i IS NOT NULL THEN
        v_DsConcepto := F_TCONCSIN_DESC_GET(pv_CdTipCon_i);
      ELSE
        v_DsConcepto := F_TMANTENI_GET_DESC('TIPOREMA'
                                           ,pv_TipoRema_i);
      END IF;
      --
      v_DsConcepto := InitCap(v_DsConcepto);
      BEGIN
        IF pv_Status_i != 'T' THEN
          n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_UnidadC
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          --
          n_ImporteC := NVL(n_ImporteC
                           ,0);
          BEGIN
            n_ImporteConvertido := n_ImporteC *
                                   f_ttipcamb_get(pv_cdmoneda_i => v_Unidad
                                                 ,pv_fecambio_i => d_FeOcurrencia);
          EXCEPTION
            WHEN OTHERS THEN
              n_ImporteConvertido := 0;
          END;
          -- -- 
          n_ImporteP := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => pv_PorTramite_i
                                       ,pv_Status_i      => pv_Status_i
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Dummy
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          n_importeP := round(NVL(n_importeP
                                 ,0)
                             ,2);
          -- --
          n_ImporteR := round(n_ImporteC - n_importeP
                             ,2);
          i          := i + 1;
          aAcumulados.extend;
          aAcumulados(i) := T_ACUMULADOS(orden      => i
                                        ,etiqueta   => v_DsConcepto
                                        ,contratado => n_ImporteC
                                        ,moneda     => v_Unidad
                                        ,otsimbol   => v_Unidad
                                        ,pagado     => n_ImporteP
                                        ,porcentaje => NULL
                                        ,remanente  => n_ImporteR
                                        ,tipo       => pv_tipo_i
                                        ,convertido => n_ImporteConvertido
                                        ,exceso     => n_ImporteExceso);
        ELSE
          n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => pv_PorTramite_i
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => NULL
                                       ,pv_Unidad_o      => v_UnidadC
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          n_ImporteT := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => pv_PorTramite_i
                                       ,pv_Status_i      => pv_Status_i
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          v_Unidad   := v_UnidadC;
          i          := i + 1;
          aAcumulados.extend;
          aAcumulados(i) := T_ACUMULADOS(orden      => i
                                        ,etiqueta   => v_DsConcepto
                                        ,contratado => ROUND(n_ImporteT
                                                            ,2)
                                        ,moneda     => v_Unidad
                                        ,otsimbol   => v_Unidad
                                        ,pagado     => ROUND(n_ImporteP
                                                            ,2)
                                        ,porcentaje => NULL
                                        ,remanente  => n_ImporteR
                                        ,tipo       => pv_tipo_i
                                        ,convertido => ROUND(n_ImporteConvertido
                                                            ,2)
                                        ,exceso     => ROUND(n_ImporteExceso
                                                            ,2));
        END IF;
      END;
    END;
  
    PROCEDURE P_ACUMULADOS_PORC IS
      CURSOR Cur_p IS
        SELECT nmcoanac
              ,nmcoaext
          FROM (SELECT T.nmcoanac
                      ,T.nmcoaext
                      ,RANK() OVER(PARTITION BY CDTIPMIX ORDER BY NMORDINA DESC) RANK_X
                  FROM TESTCOND T
                 WHERE T.ntramite = pn_ntramite_i
                   AND t.cdtipmix = 'CR')
         WHERE RANK_X = 1;
      Reg_P Cur_P%ROWTYPE;
    BEGIN
      OPEN cur_p;
      FETCH cur_p
        INTO reg_p;
      CLOSE cur_p;
      i := i + 1;
      aAcumulados.extend;
      aAcumulados(i) := T_ACUMULADOS(orden      => i
                                    ,etiqueta   => 'Coaseguro Nacional'
                                    ,contratado => NULL
                                    ,moneda     => NULL
                                    ,otsimbol   => NULL
                                    ,pagado     => NULL
                                    ,porcentaje => reg_p.nmcoanac
                                    ,remanente  => NULL
                                    ,tipo       => v_Encabezado
                                    ,convertido => n_ImporteConvertido
                                    ,exceso     => n_ImporteExceso);
      i := i + 1;
      aAcumulados.extend;
      aAcumulados(i) := T_ACUMULADOS(orden      => i
                                    ,etiqueta   => 'Coaseguro Extranjero'
                                    ,contratado => NULL
                                    ,moneda     => NULL
                                    ,otsimbol   => NULL
                                    ,pagado     => NULL
                                    ,porcentaje => reg_p.nmcoanac
                                    ,remanente  => NULL
                                    ,tipo       => v_Encabezado
                                    ,convertido => n_ImporteConvertido
                                    ,exceso     => n_ImporteExceso);
    END;
  
  BEGIN
    LOAD_DATOS(pn_ntramite_i => pn_ntramite_i);
    aAcumulados  := t_TAB_Acumulados();
    v_Encabezado := 'Importe en Transito';
    P_CONCEPTO_SET(pv_TipoRema_i   => 'SA'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => 'GHOSP'
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'SA'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => 'HMED'
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DE'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DI'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'TC'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    --
    v_Encabezado := 'Acumulado';
    P_CONCEPTO_SET(pv_TipoRema_i   => 'SA'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DE'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DI'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'TC'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    OPEN psrc_Data_o FOR
      SELECT * FROM TABLE(aAcumulados);
    RETURN psrc_Data_o;
  END;

  /*FUNCTION F_ACUMULADOS_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN SYS_REFCURSOR IS
    aAcumulados  t_tab_acumulados;
    i            PLS_INTEGER := 0;
    v_DsConcepto VARCHAR2(100);
    n_ImporteC   NUMBER;
    n_ImporteR   NUMBER;
    v_Unidad     VARCHAR2(15);
    v_Dummy      VARCHAR2(15);
    psrc_Data_o  SYS_REFCURSOR;
    n_ImporteP   NUMBER;
    PROCEDURE P_CONCEPTO_SET(pv_TipoRema_i IN VARCHAR2
                            ,pv_CdNivel_i  IN VARCHAR2) IS
    BEGIN
      v_DsConcepto := F_TMANTENI_GET_DESC('TIPOREMA'
                                         ,pv_TipoRema_i);
      BEGIN
        n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                     ,pv_CdNivel_i     => pv_CdNivel_i
                                     ,pv_cdtiporema_i  => pv_TipoRema_i
                                     ,pv_PorTramite_i  => 'N'
                                     ,pv_Status_i      => 'R'
                                     ,pv_cdtipcon_i    => NULL
                                     ,pv_Unidad_o      => v_Unidad
                                     ,pn_cdRemanente_i => NULL);
        n_ImporteP := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                     ,pv_CdNivel_i     => pv_CdNivel_i
                                     ,pv_cdtiporema_i  => pv_TipoRema_i
                                     ,pv_PorTramite_i  => 'N'
                                     ,pv_Status_i      => 'P'
                                     ,pv_cdtipcon_i    => NULL
                                     ,pv_Unidad_o      => v_Dummy
                                     ,pn_cdRemanente_i => NULL);
        n_ImporteC := NVL(n_ImporteC
                         ,0);
        n_importeP := NVL(n_importeP
                         ,0);
        n_ImporteR := n_ImporteC - n_importeP;
        i          := i + 1;
        aAcumulados.extend;
        aAcumulados(i) := T_ACUMULADOS(orden      => i
                                      ,etiqueta   => v_DsConcepto
                                      ,contratado => n_ImporteC
                                      ,moneda     => v_Unidad
                                      ,otsimbol   => v_Unidad
                                      ,pagado     => n_ImporteP
                                      ,porcentaje => NULL
                                      ,remanente  => n_ImporteR
                                      ,tipo       => NULL);
      END;
    END;
  
  BEGIN
    aAcumulados := t_TAB_Acumulados();
    P_CONCEPTO_SET('SA'
                  ,'GA');
    P_CONCEPTO_SET('DE'
                  ,'GA');
    --    P_CONCEPTO_SET('DI','GA');
    P_CONCEPTO_SET('TC'
                  ,'SI');
    OPEN psrc_Data_o FOR
      SELECT * FROM TABLE(aAcumulados);
    RETURN psrc_Data_o;
  END;*/
  FUNCTION F_PORCAPLIC_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                          ,pn_nmordina_i IN tdetimptra.nmordina%TYPE)
    RETURN NUMBER IS
    n_Return NUMBER;
  BEGIN
    BEGIN
      SELECT d.porcapli
        INTO n_Return
        FROM tdetimptra d
       WHERE d.ntramite = pn_ntramite_i
         AND d.nmordina = pn_nmordina_i;
    EXCEPTION
      WHEN no_data_found THEN
        n_Return := 0;
    END;
    RETURN n_Return;
  END;

  FUNCTION F_ACUMULADOS_DET_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN SYS_REFCURSOR IS
    aAcumuladosDet t_tab_acumulados_det;
    i              PLS_INTEGER := 0;
    v_DsConcepto   VARCHAR2(100);
    n_ImporteC     NUMBER;
    n_ImporteR     NUMBER;
    v_Unidad       VARCHAR2(15);
    v_Dummy        VARCHAR2(15);
    psrc_Data_o    SYS_REFCURSOR;
    n_ImporteP     NUMBER;
    n_TransitoGH   NUMBER;
    n_TransitoHM   NUMBER;
    n_nmordina     NUMBER;
    TYPE t_remanente IS TABLE OF NUMBER INDEX BY VARCHAR2(3);
    aValorRemanente t_remanente;
    TYPE T_TRAMITES IS TABLE OF INTEGER INDEX BY PLS_INTEGER;
    aTramTmp  T_TRAMITES;
    aTramites T_TRAMITES;
    PROCEDURE P_TRAMTMP_LOAD(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                            ,pv_CdNivel_i  IN VARCHAR2) IS
      v_SQL VARCHAR2(4000);
      CURSOR Cur_Tatrirema(pv_TpField_i IN VARCHAR2) IS
        SELECT CDATRIBU
              ,IDATRIBU
          FROM TATRIREMA
         WHERE CDNIVEL = pv_CdNivel_i
           AND tpfield = pv_TpField_i
         ORDER BY CDATRIBU;
      Reg_Tatrirema Cur_Tatrirema%ROWTYPE;
      V_SQL_G       VARCHAR2(4000);
    BEGIN
      LOAD_DATOS(pn_ntramite_i);
      v_SQL := 'SELECT NULL ';
      v_SQL := v_SQL || ' FROM TVALOREMA R ';
      v_SQL := v_SQL || 'WHERE 1=1 ';
      OPEN Cur_Tatrirema('C');
      LOOP
        FETCH Cur_Tatrirema
          INTO Reg_Tatrirema;
        EXIT WHEN Cur_Tatrirema%NOTFOUND;
        dbms_output.put_line(Reg_Tatrirema.Idatribu);
        dbms_output.put_line(pv_CdNivel_i);
        v_SQL := v_SQL || ' AND OTCLAVE' || Reg_Tatrirema.Cdatribu || '=' ||
                 CHR(39) || aVariables(Reg_Tatrirema.Idatribu) || CHR(39);
      END LOOP;
      CLOSE cur_tatrirema;
      V_SQL_G := 'SELECT TM.NTRAMITE FROM TMESACONTROL TM WHERE NTRAMITE != :NTRAMITE
      AND TM.ESTATUS IN (409
                        ,4
                        ,164
                        ,410
                        ,413
                        ,416
                        ,423
                        ,909
                        ,910)
            AND EXISTS (SELECT NULL FROM TVALOREMADET RD WHERE EXISTS (' ||
                 V_SQL ||
                 ' AND R.IDREMANENTE = RD.IDREMANENTE) AND TM.NTRAMITE = RD.NTRAMITE)';
      dbms_output.put_line(v_Sql_g);
      EXECUTE IMMEDIATE v_sql_g BULK COLLECT
        INTO aTramTmp
        USING pn_ntramite_i;
    END;
  
    PROCEDURE P_TRAMITES_LOAD(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
      CURSOR Cur_Niveles IS
        SELECT T.CODIGO
          FROM TMANTENI T
         WHERE CDTABLA = 'NIVEREMA'
           AND EXISTS (SELECT ROWNUM
                  FROM TCONFREMA C
                 WHERE CDRAMO = aVariables('CDRAMO')
                   AND T.CODIGO = C.CDNIVEL
                   AND C.SWACTIVO = K_S);
      Reg_Niveles Cur_Niveles%ROWTYPE;
      j           PLS_INTEGER := 0;
      aListTram   T_TRAMITES;
      n_Dummy     PLS_INTEGER;
    BEGIN
      OPEN Cur_Niveles;
      LOOP
        FETCH Cur_Niveles
          INTO Reg_niveles;
        EXIT WHEN Cur_Niveles%NOTFOUND;
        P_TRAMTMP_LOAD(pn_ntramite_i => pn_ntramite_i
                      ,pv_CdNivel_i  => reg_niveles.codigo);
        -- dbms_output.put_line('aTramTmp.Count:' || aTramTmp.Count);
        FOR i IN k_1 .. aTramTmp.Count LOOP
          BEGIN
            n_Dummy := aListTram(aTramTmp(i));
          EXCEPTION
            WHEN no_Data_found THEN
              aListTram(aTramTmp(i)) := aTramTmp(i);
              j := j + 1;
              aTramites(j) := aTramTmp(i);
          END;
        END LOOP;
      END LOOP;
      CLOSE Cur_Niveles;
    END;
  
    PROCEDURE P_CONCEPTO_SET(pv_TipoRema_i IN VARCHAR2
                            ,pv_CdNivel_i  IN VARCHAR2
                            ,pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
    BEGIN
      v_DsConcepto := F_TMANTENI_GET_DESC('TIPOREMA'
                                         ,pv_TipoRema_i);
      BEGIN
        n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                     ,pv_CdNivel_i     => pv_CdNivel_i
                                     ,pv_cdtiporema_i  => pv_TipoRema_i
                                     ,pv_PorTramite_i  => 'N'
                                     ,pv_Status_i      => 'R'
                                     ,pv_cdtipcon_i    => NULL
                                     ,pv_Unidad_o      => v_Unidad
                                     ,pv_nmordina_o    => n_nmordina
                                     ,pn_cdRemanente_i => NULL);
        n_ImporteC := NVL(n_ImporteC
                         ,0);
        n_ImporteP := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                     ,pv_CdNivel_i     => pv_CdNivel_i
                                     ,pv_cdtiporema_i  => pv_TipoRema_i
                                     ,pv_PorTramite_i  => k_s
                                     ,pv_Status_i      => 'P'
                                     ,pv_cdtipcon_i    => NULL
                                     ,pv_Unidad_o      => v_Dummy
                                     ,pv_nmordina_o    => n_nmordina
                                     ,pn_cdRemanente_i => NULL);
        n_importeP := NVL(n_importeP
                         ,0);
        IF aMesaControl(1).OTVALOR06 IN (1) THEN
          n_ImporteP := nvl(n_ImporteP
                           ,0) +
                        F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => k_s
                                       ,pv_Status_i      => 'D'
                                       ,pv_cdtipcon_i    => NULL
                                       ,pv_Unidad_o      => v_Dummy
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
        END IF;
        IF pv_TipoRema_i IN ('DI'
                            ,'DE') THEN
          BEGIN
            IF aValorRemanente(pv_TipoRema_i) IS NOT NULL THEN
              IF aValorRemanente(pv_TipoRema_i) > 0 THEN
                n_ImporteR := aValorRemanente(pv_TipoRema_i) - n_importeP;
              ELSE
                n_ImporteR := 0;
              END IF;
            END IF;
          EXCEPTION
            WHEN NO_dATA_FOUND THEN
              n_ImporteR := n_ImporteC - n_importeP;
          END; --  */
        ELSE
          n_ImporteR := n_ImporteC - n_importeP;
        END IF;
        aValorRemanente(pv_TipoRema_i) := n_ImporteR;
        n_transitoGH := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => k_s
                                       ,pv_Status_i      => 'T'
                                       ,pv_cdtipcon_i    => 'GHOSP'
                                       ,pv_Unidad_o      => v_Dummy
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
        n_transitoHM := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => k_s
                                       ,pv_Status_i      => 'T'
                                       ,pv_cdtipcon_i    => 'HMED'
                                       ,pv_Unidad_o      => v_Dummy
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
        IF aMesaControl(1).OTVALOR06 IN (2
                         ,8) THEN
          n_transitoGH := NVL(n_transitoGH
                             ,0) +
                          F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                         ,pv_CdNivel_i     => pv_CdNivel_i
                                         ,pv_cdtiporema_i  => pv_TipoRema_i
                                         ,pv_PorTramite_i  => k_s
                                         ,pv_Status_i      => 'D'
                                         ,pv_cdtipcon_i    => 'GHOSP'
                                         ,pv_Unidad_o      => v_Dummy
                                         ,pv_nmordina_o    => n_nmordina
                                         ,pn_cdRemanente_i => NULL);
          n_transitoHM := NVL(n_transitoHM
                             ,0) +
                          F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                         ,pv_CdNivel_i     => pv_CdNivel_i
                                         ,pv_cdtiporema_i  => pv_TipoRema_i
                                         ,pv_PorTramite_i  => k_s
                                         ,pv_Status_i      => 'D'
                                         ,pv_cdtipcon_i    => 'HMED'
                                         ,pv_Unidad_o      => v_Dummy
                                         ,pv_nmordina_o    => n_nmordina
                                         ,pn_cdRemanente_i => NULL);
        END IF;
        i := i + 1;
        aAcumuladosDET.extend;
        aAcumuladosDET(i) := T_ACUMULADOS_DET(NTRAMITE     => aMesaControl(1).OTValor15
                                             ,etiqueta     => v_DsConcepto
                                             ,contratado   => n_ImporteC
                                             ,moneda       => v_Unidad
                                             ,pagado       => n_ImporteP
                                             ,remanente    => n_ImporteR
                                             ,gstohosptran => n_TransitoGH
                                             ,honomedtran  => n_TransitoHM
                                             ,NTRAMITE_MC  => pn_ntramite_i);
      END;
    END;
  
    PROCEDURE P_CONCEPTO_MANUAL_SET(pv_TramiteLargo_i IN VARCHAR2
                                   ,pv_dsconcepto_i   IN VARCHAR2
                                   ,pn_Contratado_i   IN NUMBER
                                   ,pv_Moneda_i       IN VARCHAR2
                                   ,pn_pagado_i       IN NUMBER
                                   ,pn_Remanente_i    IN NUMBER
                                   ,pn_TransitoGH_i   IN NUMBER
                                   ,pn_TransitoHM_i   IN NUMBER
                                   ,pn_ntramite_i     IN PLS_INTEGER) IS
    BEGIN
      i := i + 1;
      aAcumuladosDET.extend;
      aAcumuladosDET(i) := T_ACUMULADOS_DET(NTRAMITE     => pv_tramitelargo_i
                                           ,etiqueta     => pv_dsconcepto_i
                                           ,contratado   => pn_Contratado_i
                                           ,moneda       => pv_Moneda_i
                                           ,pagado       => pn_pagado_i
                                           ,remanente    => pn_Remanente_i
                                           ,gstohosptran => pn_TransitoGH_i
                                           ,honomedtran  => pn_TransitoHM_i
                                           ,ntramite_mc  => pn_ntramite_i);
    END;
  
    PROCEDURE P_PORCENTAJE_COASEGURO(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
      n_Contratado            NUMBER;
      n_PorcentajeAplicadoGH  NUMBER;
      n_PorcentajeAplicadoHM  NUMBER;
      n_PorcentajeAplicadoPag NUMBER;
      n_PorcentajeAplicadoRem NUMBER;
      n_ntramite              tmesacontrol.ntramite%TYPE;
    BEGIN
      n_ntramite              := pn_ntramite_i;
      n_Contratado            := F_VTVALOGAR_GET('COASEGURO');
      n_PorcentajeAplicadoGH  := F_PORCAPLIC_GET(pn_ntramite_i => n_ntramite
                                                ,pn_nmordina_i => 7);
      n_PorcentajeAplicadoHM  := n_PorcentajeAplicadoGH;
      n_PorcentajeAplicadoPag := n_PorcentajeAplicadoGH;
      n_PorcentajeAplicadoRem := n_PorcentajeAplicadoGH;
      IF n_TransitoGH <= 0 THEN
        n_PorcentajeAplicadoGH := 0;
      END IF;
      IF n_TransitoHM <= 0 THEN
        n_PorcentajeAplicadoHM := 0;
      END IF;
      /*if n_ImporteP <= 0 then
        n_PorcentajeAplicadoPag := 0;
      end if;
      if n_ImporteR <= 0 then
        n_PorcentajeAplicadoRem := 0;
      end if;*/
      P_CONCEPTO_MANUAL_SET(pv_TramiteLargo_i => aMesaControl(1).OTValor15
                           ,pv_dsconcepto_i   => 'Coaseguro Nacional'
                           ,pn_Contratado_i   => n_Contratado
                           ,pv_Moneda_i       => '%'
                           ,pn_pagado_i       => n_PorcentajeAplicadoPag
                           ,pn_Remanente_i    => n_PorcentajeAplicadoRem
                           ,pn_TransitoGH_i   => n_PorcentajeAplicadoGH
                           ,pn_TransitoHM_i   => n_PorcentajeAplicadoHM
                           ,pn_ntramite_i     => n_ntramite);
    END;
  
    PROCEDURE P_PORCENTAJE_COASEGUROINT(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
      n_Contratado         NUMBER;
      n_PorcentajeAplicado NUMBER;
    BEGIN
      n_Contratado         := F_VTVALOGAR_GET('COASEGUROINTERNACIONAL');
      n_PorcentajeAplicado := F_PORCAPLIC_GET(pn_ntramite_i => pn_ntramite_i
                                             ,pn_nmordina_i => 7);
      P_CONCEPTO_MANUAL_SET(pv_TramiteLargo_i => aMesaControl(1).OTValor15
                           ,pv_dsconcepto_i   => 'Coaseguro Internacional'
                           ,pn_Contratado_i   => n_Contratado
                           ,pv_Moneda_i       => '%'
                           ,pn_pagado_i       => 0
                           ,pn_Remanente_i    => 0
                           ,pn_TransitoGH_i   => n_PorcentajeAplicado
                           ,pn_TransitoHM_i   => n_PorcentajeAplicado
                           ,pn_ntramite_i     => pn_ntramite_i);
    END;
  
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    P_TRAMITES_LOAD(pn_ntramite_i);
    aAcumuladosDET := t_TAB_Acumulados_DET();
    FOR i IN k_1 .. aTramites.cOUNT LOOP
      -- dbms_output.put_line('aTramites(i):' || aTramites(i));
      P_CONCEPTO_SET(pv_TipoRema_i => 'SA'
                    ,pv_CdNivel_i  => 'PA'
                    ,pn_ntramite_i => aTramites(i));
      P_CONCEPTO_SET(pv_TipoRema_i => 'DE'
                    ,pv_CdNivel_i  => 'PA'
                    ,pn_ntramite_i => aTramites(i));
      P_CONCEPTO_SET(pv_TipoRema_i => 'DI'
                    ,pv_CdNivel_i  => 'PA'
                    ,pn_ntramite_i => aTramites(i));
      -- --
      IF nvl(aMesaControl(1).otvalor45
            ,'0') = '1' THEN
        IF nvl(pkg_siniestro_func.f_ind_premier(aTramites(i))
              ,0) = 1 THEN
          P_PORCENTAJE_COASEGURO(aTramites(i));
        ELSE
          P_PORCENTAJE_COASEGUROINT(aTramites(i));
        END IF;
      ELSIF nvl(aMesaControl(1).otvalor45
               ,'0') = '0' THEN
        P_PORCENTAJE_COASEGURO(aTramites(i));
      END IF;
      -- --
      P_CONCEPTO_SET(pv_TipoRema_i => 'TC'
                    ,pv_CdNivel_i  => 'PA'
                    ,pn_ntramite_i => aTramites(i));
    END LOOP;
    OPEN psrc_Data_o FOR
      SELECT * FROM TABLE(aAcumuladosDet);
    RETURN psrc_Data_o;
  END;

  FUNCTION F_TCONCSIN_DESC_GET(pv_cdconval_i IN TCONCSIN.cdconval%TYPE)
    RETURN VARCHAR2 IS
    CURSOR Cur_DsCpto IS
      SELECT t.dsconval FROM TCONCSIN t WHERE t.cdconval = pv_cdconval_i;
    v_DsConcepto TCONCSIN.DSCONVAL%TYPE;
  BEGIN
    OPEN Cur_DsCpto;
    FETCH Cur_DsCpto
      INTO v_DsConcepto;
    CLOSE Cur_DsCpto;
    RETURN v_DsConcepto;
  END;

  FUNCTION F_ACUMULADOS_POST_CALC_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN SYS_REFCURSOR IS
    aAcumulados  t_tab_acumulados;
    i            PLS_INTEGER := 0;
    v_DsConcepto VARCHAR2(100);
    n_ImporteC   NUMBER;
    n_ImporteT   NUMBER;
    n_ImporteR   NUMBER;
    n_ImporteD   NUMBER;
    v_Unidad     VARCHAR2(15);
    v_UnidadC    VARCHAR2(15);
    v_Dummy      VARCHAR2(15);
    psrc_Data_o  SYS_REFCURSOR;
    n_ImporteP   NUMBER;
    v_Encabezado VARCHAR2(200);
    --
    n_ImporteConvertido NUMBER;
    n_ImporteExceso     NUMBER := 0;
    n_nmordina          NUMBER;
    --
    PROCEDURE P_CONCEPTO_SET(pv_TipoRema_i   IN VARCHAR2
                            ,pv_CdNivel_i    IN VARCHAR2
                            ,pv_Status_i     IN VARCHAR2 DEFAULT 'P'
                            ,pv_PorTramite_i IN VARCHAR2 DEFAULT 'N'
                            ,pv_CdTipCon_i   IN VARCHAR2
                            ,pv_Tipo_i       IN VARCHAR2) IS
      FUNCTION F_TCONCSIN_DESC_GET(pv_cdtipcon_i IN TCONCSIN.cdtipcon%TYPE)
        RETURN VARCHAR2 IS
        CURSOR Cur_DsCpto IS
          SELECT t.dsconval
            FROM TCONCSIN t
           WHERE t.cdtipcon = pv_cdtipcon_i;
        v_DsConcepto TCONCSIN.DSCONVAL%TYPE;
      BEGIN
        OPEN Cur_DsCpto;
        FETCH Cur_DsCpto
          INTO v_DsConcepto;
        CLOSE Cur_DsCpto;
        RETURN v_DsConcepto;
      END;
    
    BEGIN
      IF pv_CdTipCon_i IS NOT NULL THEN
        v_DsConcepto := F_TCONCSIN_DESC_GET(pv_CdTipCon_i);
      ELSE
        v_DsConcepto := F_TMANTENI_GET_DESC('TIPOREMA'
                                           ,pv_TipoRema_i);
      END IF;
      --
      v_DsConcepto := InitCap(v_DsConcepto);
      BEGIN
        IF pv_Status_i != 'T' THEN
          n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          n_ImporteP := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => pv_Status_i
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Dummy
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          IF aMesaControl(1).OTVALOR06 IN (1) THEN
            n_ImporteD := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                         ,pv_CdNivel_i     => pv_CdNivel_i
                                         ,pv_cdtiporema_i  => pv_TipoRema_i
                                         ,pv_PorTramite_i  => pv_PorTramite_i
                                         ,pv_Status_i      => 'D'
                                         ,pv_cdtipcon_i    => pv_CdTipCon_i
                                         ,pv_Unidad_o      => v_Dummy
                                         ,pv_nmordina_o    => n_nmordina
                                         ,pn_cdRemanente_i => NULL);
            n_ImporteP := NVL(n_ImporteP
                             ,0) + NVL(n_ImporteD
                                      ,0);
          END IF;
          n_ImporteC := NVL(n_ImporteC
                           ,0);
          n_importeP := NVL(n_importeP
                           ,0);
          n_ImporteR := n_ImporteC - n_importeP;
          i          := i + 1;
          aAcumulados.extend;
          aAcumulados(i) := T_ACUMULADOS(orden      => i
                                        ,etiqueta   => v_DsConcepto
                                        ,contratado => n_ImporteC
                                        ,moneda     => v_Unidad
                                        ,otsimbol   => v_Unidad
                                        ,pagado     => n_ImporteP
                                        ,porcentaje => NULL
                                        ,remanente  => n_ImporteR
                                        ,tipo       => pv_tipo_i
                                        ,convertido => n_ImporteConvertido
                                        ,exceso     => n_ImporteExceso);
        ELSE
          n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => NULL
                                       ,pv_Unidad_o      => v_UnidadC
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          n_ImporteT := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => pv_Status_i
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          v_Unidad   := v_UnidadC;
          IF aMesaControl(1).OTVALOR06 IN (2
                           ,5
                           ,6
                           ,7
                           ,8) THEN
            n_ImporteD := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                         ,pv_CdNivel_i     => pv_CdNivel_i
                                         ,pv_cdtiporema_i  => pv_TipoRema_i
                                         ,pv_PorTramite_i  => pv_PorTramite_i
                                         ,pv_Status_i      => 'D'
                                         ,pv_cdtipcon_i    => pv_CdTipCon_i
                                         ,pv_Unidad_o      => v_Dummy
                                         ,pv_nmordina_o    => n_nmordina
                                         ,pn_cdRemanente_i => NULL);
            n_ImporteC := NVL(n_ImporteT
                             ,0) + NVL(n_ImporteD
                                      ,0);
            n_importeP := 0;
            n_ImporteR := 0;
          ELSE
            n_ImporteC := n_ImporteT;
          END IF;
          i := i + 1;
          aAcumulados.extend;
          aAcumulados(i) := T_ACUMULADOS(orden      => i
                                        ,etiqueta   => v_DsConcepto
                                        ,contratado => n_ImporteC
                                        ,moneda     => v_Unidad
                                        ,otsimbol   => v_Unidad
                                        ,pagado     => n_ImporteP
                                        ,porcentaje => NULL
                                        ,remanente  => n_ImporteR
                                        ,tipo       => pv_tipo_i
                                        ,convertido => n_ImporteConvertido
                                        ,exceso     => n_ImporteExceso);
        END IF;
      END;
    END;
  
    PROCEDURE P_ACUMULADOS_PORC IS
      CURSOR Cur_p IS
        SELECT nmcoanac
              ,nmcoaext
          FROM (SELECT T.nmcoanac
                      ,T.nmcoaext
                      ,RANK() OVER(PARTITION BY CDTIPMIX ORDER BY NMORDINA DESC) RANK_X
                  FROM TESTCOND T
                 WHERE T.ntramite = pn_ntramite_i
                   AND t.cdtipmix = 'CR')
         WHERE RANK_X = 1;
      Reg_P Cur_P%ROWTYPE;
    BEGIN
      OPEN cur_p;
      FETCH cur_p
        INTO reg_p;
      CLOSE cur_p;
      i := i + 1;
      aAcumulados.extend;
      aAcumulados(i) := T_ACUMULADOS(orden      => i
                                    ,etiqueta   => 'Coaseguro Nacional'
                                    ,contratado => NULL
                                    ,moneda     => NULL
                                    ,otsimbol   => NULL
                                    ,pagado     => NULL
                                    ,porcentaje => reg_p.nmcoanac
                                    ,remanente  => NULL
                                    ,tipo       => v_Encabezado
                                    ,convertido => n_ImporteConvertido
                                    ,exceso     => n_ImporteExceso);
      i := i + 1;
      aAcumulados.extend;
      aAcumulados(i) := T_ACUMULADOS(orden      => i
                                    ,etiqueta   => 'Coaseguro Extranjero'
                                    ,contratado => NULL
                                    ,moneda     => NULL
                                    ,otsimbol   => NULL
                                    ,pagado     => NULL
                                    ,porcentaje => reg_p.nmcoanac
                                    ,remanente  => NULL
                                    ,tipo       => v_Encabezado
                                    ,convertido => n_ImporteConvertido
                                    ,exceso     => n_ImporteExceso);
    END;
  
  BEGIN
    LOAD_DATOS(pn_ntramite_i => pn_ntramite_i);
    aAcumulados  := t_TAB_Acumulados();
    v_Encabezado := 'Importe en Transito';
    P_CONCEPTO_SET('SA'
                  ,'PA'
                  ,'T'
                  ,k_s
                  ,'GHOSP'
                  ,v_Encabezado);
    P_CONCEPTO_SET('SA'
                  ,'PA'
                  ,'T'
                  ,k_s
                  ,'HMED'
                  ,v_Encabezado);
    P_CONCEPTO_SET('DE'
                  ,'PA'
                  ,'T'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    P_CONCEPTO_SET('DI'
                  ,'PA'
                  ,'T'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    P_CONCEPTO_SET('TC'
                  ,'PA'
                  ,'T'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    --
    v_Encabezado := 'Acumulado';
    P_CONCEPTO_SET('SA'
                  ,'PA'
                  ,'P'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    P_CONCEPTO_SET('DE'
                  ,'PA'
                  ,'P'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    P_CONCEPTO_SET('DI'
                  ,'PA'
                  ,'P'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    P_CONCEPTO_SET('TC'
                  ,'PA'
                  ,'P'
                  ,k_s
                  ,NULL
                  ,v_Encabezado);
    OPEN psrc_Data_o FOR
      SELECT * FROM TABLE(aAcumulados);
    RETURN psrc_Data_o;
  END;

  FUNCTION F_REACTIVAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                      ,pv_CdConval_i IN TVALOREMADET.cdconval%TYPE
                      ,pv_CdTipCon_i IN TCONCSIN.CDTIPCON%TYPE
                      ,pv_status_i   IN TVALOREMADET.STATUS%TYPE)
    RETURN VARCHAR2 IS
    n_Suma NUMBER;
    CURSOR Cur_D IS
      SELECT idremanente
            ,nmordina
        FROM tvaloremadet d
       WHERE d.ntramite = pn_ntramite_i
         AND (d.cdconval = pv_CdConval_i OR pv_CdConval_i IS NULL)
         AND (d.cdconval IN
             (SELECT CDCONVAL FROM TCONCSIN WHERE CDTIPCON = pv_CdTipCon_i) OR
             pv_CdTipCon_i IS NULL)
         AND d.status IN ('T'
                         ,'P');
    Reg_D Cur_D%ROWTYPE;
  BEGIN
    OPEN cur_d;
    LOOP
      FETCH cur_d
        INTO reg_d;
      EXIT WHEN cur_D%NOTFOUND;
      UPDATE tvaloremadet d
         SET D.STATUS   = pv_status_i
            ,d.feSTATUS = SYSDATE
       WHERE idremanente = Reg_D.Idremanente
         AND nmordina = Reg_D.nmordina;
      --Sumamos los Registrados - los pagados
      n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => Reg_D.Idremanente
                                   ,pv_Status_i      => 'R') -
                F_SUM_OTVALOR01_GET(pn_IdRemanente_i => Reg_D.Idremanente
                                   ,pv_Status_i      => 'P');
      --Actualizamos el Remanente con la suma del detalle
      P_TVALOREMA_UPD(pn_IdRemanente_i => Reg_D.Idremanente
                     ,pv_OtValor01_i   => n_Suma);
    END LOOP;
    CLOSE cur_D;
    RETURN 'ORDS-1002';
  EXCEPTION
    WHEN OTHERS THEN
      Pkg_Traduc.p_inserta_bitacora(pi_msg_id   => 1
                                   ,pi_code_err => SQLCODE
                                   ,pi_msg_text => SQLERRM
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'PKG_REMANENTE.F_REACTIVAR'
                                   ,pi_tipo     => 'Error');
      RETURN 'ORDS-2001';
  END;

  FUNCTION F_ARMA_QUERY(pv_CdNivel_i  IN VARCHAR2
                       ,pv_CdConVal_i IN VARCHAR2 DEFAULT NULL)
    RETURN VARCHAR2 IS
    v_SQL VARCHAR2(4000);
    CURSOR Cur_Tatrirema(pv_TpField_i IN VARCHAR2) IS
      SELECT CDATRIBU
            ,IDATRIBU
        FROM TATRIREMA
       WHERE CDNIVEL = pv_CdNivel_i
         AND tpfield = pv_TpField_i
       ORDER BY CDATRIBU;
    Reg_Tatrirema Cur_Tatrirema%ROWTYPE;
    b_Existe      BOOLEAN := FALSE;
  BEGIN
    v_SQL := 'SELECT R.idremanente,R.cdnivel,C.cdconval ';
    v_SQL := v_SQL ||
             'FROM TVALOREMA R JOIN TCONFREMA C ON C.CDREMANENTE = R.CDREMANENTE ';
    v_SQL := v_SQL || ' AND C.cdramo = ' || CHR(39) || aVariables('CDRAMO') ||
             chr(39);
    v_SQL := v_SQL || ' WHERE C.CDTIPOREMA = :1';
    IF pv_CdConVal_i IS NOT NULL THEN
      v_SQL := v_SQL || ' AND C.cdconval = ' || CHR(39) || pv_CdConVal_i ||
               chr(39);
    END IF;
    v_SQL := v_SQL || ' AND C.SWACTIVO = ' || CHR(39) || K_S || chr(39);
    OPEN Cur_Tatrirema('C');
    LOOP
      FETCH Cur_Tatrirema
        INTO Reg_Tatrirema;
      EXIT WHEN Cur_Tatrirema%NOTFOUND;
      b_Existe := TRUE;
      v_SQL    := v_SQL || ' AND OTCLAVE' || Reg_Tatrirema.Cdatribu ||
                  '= aVariables(' || chr(39) || Reg_Tatrirema.Idatribu ||
                  chr(39) || ')';
    END LOOP;
    CLOSE cur_tatrirema;
    IF NOT b_Existe THEN
      v_Sql := NULL;
    END IF;
    RETURN v_SQL;
  END;

  PROCEDURE P_LOAD_CONTADORES(pv_CdConVal_i IN VARCHAR2 DEFAULT NULL) IS
    CURSOR Cur_CF IS
      SELECT DISTINCT C.CDNIVEL
        FROM tconfrema c
       WHERE c.cdramo = aVariables('CDRAMO')
         AND c.cdtiporema = 'CO'
         AND C.SWACTIVO = K_S;
    Reg_Cf        Cur_Cf%ROWTYPE;
    v_SQL         VARCHAR2(4000);
    n_IdRemanente PLS_INTEGER;
    v_CdNivel     VARCHAR2(2);
    v_cdconval    VARCHAR2(20);
    j             PLS_INTEGER := 0;
    cur_rema      SYS_REFCURSOR;
    PROCEDURE P_SUSTITUIR_VARIABLES IS
    BEGIN
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDUNIECO' || chr(39) || ')'
                      ,chr(39) || aVariables('CDUNIECO') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDRAMO' || chr(39) || ')'
                      ,chr(39) || aVariables('CDRAMO') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'ESTADO' || chr(39) || ')'
                      ,chr(39) || aVariables('ESTADO') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'NMPOLIZA' || chr(39) || ')'
                      ,chr(39) || aVariables('NMPOLIZA') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'NMSUPLEM' || chr(39) || ')'
                      ,chr(39) || aVariables('NMSUPLEM') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'NMSITUAC' || chr(39) || ')'
                      ,chr(39) || aVariables('NMSITUAC') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDGARANT' || chr(39) || ')'
                      ,chr(39) || aVariables('CDGARANT') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDICD' || chr(39) || ')'
                      ,chr(39) || aVariables('CDICD') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'NMNEGOCIO' || chr(39) || ')'
                      ,chr(39) || aVariables('NMNEGOCIO') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDANIO' || chr(39) || ')'
                      ,chr(39) || aVariables('CDANIO') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'NMEMPRESA' || chr(39) || ')'
                      ,chr(39) || aVariables('NMEMPRESA') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'NMNEGPOL' || chr(39) || ')'
                      ,chr(39) || aVariables('NMNEGPOL') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDFILIAL' || chr(39) || ')'
                      ,chr(39) || aVariables('CDFILIAL') || chr(39));
      v_SQL := REPLACE(v_sql
                      ,'aVariables(' || chr(39) || 'CDGRUPO' || chr(39) || ')'
                      ,chr(39) || aVariables('CDGRUPO') || chr(39));
    END;
  
  BEGIN
    OPEN Cur_CF;
    LOOP
      FETCH Cur_CF
        INTO Reg_Cf;
      EXIT WHEN Cur_CF%NOTFOUND;
      v_SQL := F_ARMA_QUERY(pv_CdNivel_i  => Reg_Cf.Cdnivel
                           ,pv_CdConVal_i => pv_CdConVal_i);
      IF v_SQL IS NOT NULL THEN
        P_SUSTITUIR_VARIABLES;
        -- dbms_output.put_line(v_Sql);
        OPEN cur_rema FOR v_SQL
          USING 'CO';
        LOOP
          FETCH cur_rema
            INTO n_idRemanente
                ,v_CdNivel
                ,v_cdconval;
          EXIT WHEN cur_rema%NOTFOUND;
          j := j + 1;
          aRemaCont(j).IdRemanente := n_idRemanente;
          aRemaCont(j).CdNivel := v_CdNivel;
          aRemaCont(j).cdconval := v_cdconval;
        END LOOP;
        CLOSE cur_rema;
      END IF;
    END LOOP;
    CLOSE cur_Cf;
  END;

  FUNCTION F_CONTADORES_GET(pn_ntramite_i           IN tmesacontrol.ntramite%TYPE
                           ,pv_SumarTramiteActual_i IN VARCHAR2 DEFAULT 'N'
                           ,pv_CdConVal_i           IN VARCHAR2 DEFAULT NULL
                           ,pv_CdTipCon_i           IN VARCHAR2 DEFAULT NULL)
    RETURN SYS_REFCURSOR IS
    aContadores  t_tab_contadores;
    i            PLS_INTEGER := 0;
    v_DsTipo     VARCHAR2(100);
    n_ImporteC   NUMBER;
    n_ImporteR   NUMBER;
    v_Unidad     VARCHAR2(50);
    v_nmordina   number;
    v_ordinal    number;
    v_UnidadCont VARCHAR2(50);
    psrc_Data_o  SYS_REFCURSOR;
    n_ImporteP   NUMBER;
    n_Dictamen   NUMBER;
    PROCEDURE P_CONCEPTO_SET(pn_IdRemanente_i IN PLS_INTEGER
                            ,pv_cdNivel_i     IN VARCHAR2
                            ,pv_cdconval_i    IN VARCHAR2) IS
      v_DsCont VARCHAR2(100);
    BEGIN
      v_DsCont := F_TCONCSIN_DESC_GET(pv_cdconval_i);
      v_DsTipo := F_TMANTENI_GET_DESC('NIVEREMA'
                                     ,pv_cdNivel_i);
      BEGIN
       v_ordinal := 0;
        n_ImporteC   := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_cdNivel_i
                                       ,pv_cdtiporema_i  => 'CO'
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => NULL
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => v_nmordina
                                       ,pn_cdRemanente_i => NULL
                                       ,pv_CdTipo_i      => NULL
                                       ,pn_IdRemanente_i => pn_IdRemanente_i);
        v_UnidadCont := v_Unidad;
        v_ordinal    := nvl(v_nmordina,0);
        n_ImporteP   := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_cdNivel_i
                                       ,pv_cdtiporema_i  => 'CO'
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => 'P'
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => v_nmordina
                                       ,pn_cdRemanente_i => NULL
                                       ,pv_CdTipo_i      => NULL
                                       ,pn_IdRemanente_i => pn_IdRemanente_i);
        n_ImporteC   := NVL(n_ImporteC
                           ,0);
        n_importeP   := NVL(n_importeP
                           ,0);
                           
        
        IF pv_SumarTramiteActual_i = k_s THEN
          n_Dictamen := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                       ,pv_CdNivel_i     => pv_cdNivel_i
                                       ,pv_cdtiporema_i  => 'CO'
                                       ,pv_PorTramite_i  => k_s
                                       ,pv_Status_i      => 'D'
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => v_nmordina
                                       ,pn_cdRemanente_i => NULL
                                       ,pv_CdTipo_i      => NULL
                                       ,pn_IdRemanente_i => pn_IdRemanente_i);
        END IF;
        n_ImporteR := n_ImporteC - n_importeP -
                      nvl(n_Dictamen
                         ,0);
        i          := i + 1;
        aContadores.extend;
        aContadores(i) := t_contadores(tipo        => v_DsTipo
                                      ,orden       => i
                                      ,Contador    => v_DsCont
                                      ,unidad      => v_UnidadCont
                                      ,contratado  => n_ImporteC
                                      ,consumido   => n_ImporteP
                                      ,disponible  => n_ImporteR
                                      ,idremanente => pn_IdRemanente_i
                                      ,cdconval    => pv_cdconval_i
                                      ,nmordina    => v_nmordina);
      END;
    END;
  
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    P_LOAD_CONTADORES(pv_CdConVal_i => pv_CdConVal_i);
    aContadores := t_TAB_Contadores();
    FOR i IN k_1 .. aRemaCont.Count LOOP
      P_CONCEPTO_SET(aRemaCont(i).IdRemanente
                    ,aRemaCont(i).CdNivel
                    ,aRemaCont(i).cdconval);
    END LOOP;
    OPEN psrc_Data_o FOR
      SELECT tipo        AS nivel
            ,contador    AS dsatributo
            ,unidad      AS unidad
            ,contratado  AS contratado
            ,consumido   AS consumido
            ,disponible  AS cdremanente
            ,idremanente AS idremanente
            ,cdconval    AS cdconval
            ,v_ordinal   AS nmordinal
        FROM TABLE(aContadores);
    RETURN psrc_Data_o;
  END;

  FUNCTION F_REMANENTE_CONT_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                               ,pv_CdConVal_i IN VARCHAR2) RETURN NUMBER IS
    Cur_Cont    SYS_REFCURSOR;
    v_Dummy     VARCHAR2(200);
    n_Dummy     PLS_INTEGER;
    n_Remanente NUMBER;
  BEGIN
    Cur_Cont := PKG_REMANENTE.F_CONTADORES_GET(pn_ntramite_i           => pn_ntramite_i
                                              ,pv_SumarTramiteActual_i => 'N'
                                              ,pv_CdConVal_i           => pv_CdConVal_i
                                              ,pv_CdTipCon_i           => NULL);
    FETCH cur_cont
      INTO v_Dummy
          ,v_Dummy
          ,v_Dummy
          ,n_Dummy
          ,n_Dummy
          ,n_Remanente
          ,n_Dummy
          ,v_Dummy;
    CLOSE cur_cont;
    n_Remanente := nvl(n_Remanente
                      ,999999999999999);
    RETURN n_Remanente;
  END;

  FUNCTION F_RECALCULAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                       ,pv_CdConval_i IN TVALOREMADET.cdconval%TYPE
                       ,pv_CdTipCon_i IN TCONCSIN.CDTIPCON%TYPE
                       ,pv_status_i   IN TVALOREMADET.STATUS%TYPE)
    RETURN VARCHAR2 IS
    n_Dummy NUMBER;
  BEGIN
    n_Dummy := pkg_msiniest_cal.f_calc_importes_get(pn_ntramite_i
                                                   ,NULL
                                                   ,0
                                                   ,0);
    n_Dummy := f_reactivar(pn_ntramite_i => pn_ntramite_i
                          ,pv_CdConval_i => pv_CdConval_i
                          ,pv_CdTipCon_i => pv_cdtipcon_i
                          ,pv_status_i   => pv_status_i);
    P_TVALOREMADET_DEL(pn_ntramite_i
                      ,'D');
    p_registrar(pn_ntramite_i);
    RETURN 'ORDS-1002';
  EXCEPTION
    WHEN OTHERS THEN
      Pkg_Traduc.p_inserta_bitacora(pi_msg_id   => 1
                                   ,pi_code_err => SQLCODE
                                   ,pi_msg_text => SQLERRM
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'PKG_REMANENTE.F_RECARCULAR'
                                   ,pi_tipo     => 'Error');
      RETURN 'ORDS-2001';
  END F_RECALCULAR;

  FUNCTION F_APROBAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN VARCHAR2 IS
  BEGIN
    P_APROBAR(pn_ntramite_i => pn_ntramite_i);
    RETURN 'ORDS-1003';
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 'ORDS-2001';
  END;

  FUNCTION F_PAGAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                  ,pv_CdConval_i IN TVALOREMADET.cdconval%TYPE
                  ,pv_CdTipCon_i IN TCONCSIN.CDTIPCON%TYPE
                  ,pn_monto_i    IN TVALOREMADET.OTVALOR01%TYPE)
    RETURN VARCHAR2 IS
    n_Suma NUMBER;
    CURSOR Cur_D IS
      SELECT idremanente
            ,nmordina
            ,D.OTVALOR01
        FROM tvaloremadet d
       WHERE d.ntramite = pn_ntramite_i
         AND (d.cdconval = pv_CdConval_i OR pv_CdConval_i IS NULL)
         AND (d.cdconval IN
             (SELECT CDCONVAL FROM TCONCSIN WHERE CDTIPCON = pv_CdTipCon_i) OR
             pv_CdTipCon_i IS NULL)
         AND d.status IN ('T');
    Reg_D      Cur_D%ROWTYPE;
    aValores   t_100_30;
    n_NmOrdina PLS_INTEGER;
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    OPEN cur_d;
    LOOP
      FETCH cur_d
        INTO reg_d;
      EXIT WHEN cur_D%NOTFOUND;
      IF aMesaControl(1).OtValor06 = 1 THEN
        UPDATE tvaloremadet d
           SET D.STATUS   = 'P'
              ,d.feSTATUS = SYSDATE
         WHERE idremanente = Reg_D.Idremanente
           AND nmordina = Reg_D.nmordina;
      ELSE
        aValores('OTVALOR01') := to_number(pn_monto_i);
        aValores('OTVALOR02') := f_unidad_get(Reg_D.Idremanente);
        n_NmOrdina := F_MAX_NUMORD(pn_IdRemanente_i => Reg_D.Idremanente) + 1;
        P_TVALOREMADET_INS(pn_IdRemanente_i => Reg_D.Idremanente
                          ,pn_NmOrdina_i    => n_NmOrdina
                          ,pv_CdConval_i    => pv_CdConval_i
                          ,pv_Status_i      => 'P'
                          ,pd_FESTATUS_i    => SYSDATE
                          ,pv_OtValor01_i   => aValores('OTVALOR01')
                          ,pv_OtValor02_i   => aValores('OTVALOR02'));
        aValores('OTVALOR01') := to_number(pn_monto_i) * -1;
        n_NmOrdina := F_MAX_NUMORD(pn_IdRemanente_i => Reg_D.Idremanente) + 1;
        P_TVALOREMADET_INS(pn_IdRemanente_i => Reg_D.Idremanente
                          ,pn_NmOrdina_i    => n_NmOrdina
                          ,pv_CdConval_i    => pv_CdConval_i
                          ,pv_Status_i      => 'T'
                          ,pd_FESTATUS_i    => SYSDATE
                          ,pv_OtValor01_i   => aValores('OTVALOR01')
                          ,pv_OtValor02_i   => aValores('OTVALOR02'));
      END IF;
      --Sumamos los Registrados - los pagados
      n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => Reg_D.Idremanente
                                   ,pv_Status_i      => 'R') -
                F_SUM_OTVALOR01_GET(pn_IdRemanente_i => Reg_D.Idremanente
                                   ,pv_Status_i      => 'P');
      --Actualizamos el Remanente con la suma del detalle
      P_TVALOREMA_UPD(pn_IdRemanente_i => Reg_D.Idremanente
                     ,pv_OtValor01_i   => n_Suma);
    END LOOP;
    CLOSE cur_D;
    RETURN 'ORDS-1002';
  EXCEPTION
    WHEN OTHERS THEN
      Pkg_Traduc.p_inserta_bitacora(pi_msg_id   => 1
                                   ,pi_code_err => SQLCODE
                                   ,pi_msg_text => SQLERRM
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'PKG_REMANENTE.F_REACTIVAR'
                                   ,pi_tipo     => 'Error');
      RETURN 'ORDS-2001';
  END;

  FUNCTION F_CANCELAR(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                     ,pv_CdConval_i IN TVALOREMADET.cdconval%TYPE
                     ,pv_CdTipCon_i IN TCONCSIN.CDTIPCON%TYPE
                     ,pn_monto_i    IN NUMBER) RETURN VARCHAR2 IS
    CURSOR Cur_D IS
      SELECT idremanente
            ,nmordina
        FROM tvaloremadet d
       WHERE d.ntramite = pn_ntramite_i
         AND (d.cdconval = pv_CdConval_i OR pv_CdConval_i IS NULL)
         AND (d.cdconval IN
             (SELECT CDCONVAL FROM TCONCSIN WHERE CDTIPCON = pv_CdTipCon_i) OR
             pv_CdTipCon_i IS NULL)
         AND d.status IN ('D');
    Reg_D Cur_D%ROWTYPE;
  BEGIN
    OPEN cur_d;
    LOOP
      FETCH cur_d
        INTO reg_d;
      EXIT WHEN cur_D%NOTFOUND;
      UPDATE tvaloremadet d
         SET D.STATUS   = 'C'
            ,d.feSTATUS = SYSDATE
       WHERE idremanente = Reg_D.Idremanente
         AND nmordina = Reg_D.nmordina;
    END LOOP;
    CLOSE cur_D;
    RETURN 'ORDS-1002';
  EXCEPTION
    WHEN OTHERS THEN
      Pkg_Traduc.p_inserta_bitacora(pi_msg_id   => 1
                                   ,pi_code_err => SQLCODE
                                   ,pi_msg_text => SQLERRM
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'PKG_REMANENTE.F_REACTIVAR'
                                   ,pi_tipo     => 'Error');
      RETURN 'ORDS-2001';
  END;

  FUNCTION F_PAG_APROB_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN SYS_REFCURSOR IS
    aPagAprob    t_tab_pag_aprob2;
    i            PLS_INTEGER := 0;
    v_DsConcepto VARCHAR2(100);
    v_Dummy      VARCHAR2(15);
    psrc_Data_o  SYS_REFCURSOR;
    n_ImporteP   NUMBER;
    --
    --
    CURSOR Cur_Tramites IS
      SELECT ntramite
        FROM (SELECT DISTINCT icd.ntramite
                FROM msiniicd icd
                JOIN tmesacontrol tme
                  ON icd.cdunieco = tme.cdunieco
                 AND icd.cdramo = tme.cdramo
                 AND icd.aaapertu = tme.otvalor24
                 AND icd.nmsinies = tme.otvalor25
               WHERE tme.ntramite = pn_ntramite_i) q
       WHERE ntramite != pn_ntramite_i
       ORDER BY ntramite;
    Reg_Tramites Cur_Tramites%ROWTYPE;
    --
    CURSOR Cur_Mpa(pn_ntramite_i IN tmesacontrol.ntramite%TYPE) IS
      SELECT mpa.swestado
            ,mpa.cdmoneda
        FROM mpagosin mpa
       WHERE mpa.ntramite = pn_ntramite_i
         AND mpa.cdunieco = aVariables('CDUNIECO')
         AND mpa.cdramo = aVariables('CDRAMO')
         AND mpa.aaapertu = aVariables('AAAPERTU')
         AND mpa.status = aVariables('STATUS')
         AND mpa.nmsinies = aVariables('NMSINIES');
    --
    d_FeOcurrencia DATE;
    --
    n_Tasa NUMBER;
    FUNCTION F_ESTADO_GET(pv_status_i IN VARCHAR2) RETURN VARCHAR2 IS
      v_return VARCHAR2(100);
    BEGIN
      BEGIN
        SELECT dsestadomc
          INTO v_return
          FROM testadomc d
         WHERE d.estatus = pv_status_i;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          v_return := NULL;
      END;
      RETURN v_return;
    END;
  
    FUNCTION F_APLICADO_GET(pv_TipoRema_i IN VARCHAR2
                           ,pv_CdNivel_i  IN VARCHAR2
                           ,pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
      RETURN NUMBER IS
      n_ImporteP NUMBER;
      n_nmordina NUMBER;
    BEGIN
      v_DsConcepto := F_TMANTENI_GET_DESC('TIPOREMA'
                                         ,pv_TipoRema_i);
      BEGIN
        n_ImporteP := F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                     ,pv_CdNivel_i     => pv_CdNivel_i
                                     ,pv_cdtiporema_i  => pv_TipoRema_i
                                     ,pv_PorTramite_i  => k_s
                                     ,pv_Status_i      => 'P'
                                     ,pv_cdtipcon_i    => NULL
                                     ,pv_Unidad_o      => v_Dummy
                                     ,pv_nmordina_o    => n_nmordina
                                     ,pn_cdRemanente_i => NULL);
        n_importeP := NVL(n_importeP
                         ,0);
      END;
      RETURN n_ImporteP;
    END;
  
    FUNCTION F_IDREMANENTE_GET(pv_TipoRema_i IN VARCHAR2
                              ,pn_Ntramite_i IN tmesacontrol.ntramite%TYPE
                              ,pv_cdtipo_i   IN VARCHAR2 DEFAULT 'P')
      RETURN PLS_INTEGER IS
      CURSOR Cur_R IS
        SELECT R.IDREMANENTE
          FROM TVALOREMA R
         WHERE EXISTS (SELECT NULL
                  FROM TVALOREMADET D
                 WHERE D.NTRAMITE = pn_Ntramite_i
                   AND R.IDREMANENTE = D.IDREMANENTE)
           AND EXISTS (SELECT NULL
                  FROM TCONFREMA C
                 WHERE C.CDREMANENTE = R.CDREMANENTE
                   AND C.CDTIPOREMA = pv_TipoRema_i
                   AND C.SWACTIVO = K_S);
    BEGIN
      BEGIN
        OPEN cur_r;
        FETCH cur_r
          INTO aVariables('IDREMANENTE');
        CLOSE cur_r;
        dbms_output.put_line('pv_TipoRema_i:' || pv_TipoRema_i);
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          aVariables('IDREMANENTE') := NULL;
      END;
      RETURN TO_NUMBER(aVariables('IDREMANENTE'));
    END;
  
  BEGIN
    aPagAprob := t_tab_pag_aprob2();
    OPEN Cur_Tramites;
    LOOP
      FETCH Cur_Tramites
        INTO Reg_Tramites;
      EXIT WHEN Cur_Tramites%NOTFOUND;
      -- dbms_output.put_line(Reg_Tramites.Ntramite);
      LOAD_DATOS(pn_ntramite_i => Reg_Tramites.Ntramite);
      aVariables('CDESTADO') := F_ESTADO_GET(aMesaControl(1).estatus);
      dbms_output.put_line(Reg_Tramites.Ntramite);
      OPEN Cur_Mpa(Reg_Tramites.Ntramite);
      FETCH Cur_Mpa
        INTO aVariables('CDESTADOPAG')
            ,aVariables('MONEDAPAGO');
      IF CUR_MPA%NOTFOUND THEN
        aVariables('CDESTADOPAG') := 1;
        aVariables('MONEDAPAGO') := 'MXN';
      END IF;
      CLOSE Cur_Mpa;
      aVariables('COASEGUROAPLICADO') := F_APLICADO_GET(pv_TipoRema_i => 'TC'
                                                       ,pv_CdNivel_i  => 'PA'
                                                       ,pn_ntramite_i => Reg_Tramites.Ntramite);
      aVariables('DEDUCIBLEAPLICADO') := F_APLICADO_GET(pv_TipoRema_i => 'DE'
                                                       ,pv_CdNivel_i  => 'PA'
                                                       ,pn_ntramite_i => Reg_Tramites.Ntramite);
      aVariables('IMPPAGOCONT') := F_APLICADO_GET(pv_TipoRema_i => 'SA'
                                                 ,pv_CdNivel_i  => 'PA'
                                                 ,pn_ntramite_i => Reg_Tramites.Ntramite);
      --
      d_FeOcurrencia := PKG_REGLAS_CAL.F_OCURRENCIA_TRAMITE_GET(pn_ntramite_i => pn_ntramite_i);
      --
      aVariables('UNDPAGOCONT') := f_unidad_get(pn_IDRemanente_i => F_IDREMANENTE_GET('SA'
                                                                                     ,Reg_Tramites.Ntramite));
      --  n_Tasa := f_ttipcamb_get(aVariables('UNDPAGOCONT'),d_FeOcurrencia);
      --aVariables('IMPPAGOPESO') := TO_NUMBER(aVariables('IMPPAGOCONT')) * n_Tasa; --Sa Aplicado en moneda contratada
      --
      aVariables('IMPPAGOPESO') := TO_NUMBER(aVariables('IMPPAGOCONT')); --Sa Aplicado en moneda contratada
      aVariables('UNDDEDUCCONT') := f_unidad_get(pn_IDRemanente_i => F_IDREMANENTE_GET('DE'
                                                                                      ,Reg_Tramites.Ntramite));
      n_Tasa := f_ttipcamb_get(aVariables('UNDDEDUCCONT')
                              ,d_FeOcurrencia);
      aVariables('DEDUUNCONT') := TO_NUMBER(aVariables('DEDUCIBLEAPLICADO')) *
                                  n_Tasa; --Dedu Aplicado en moneda contratada
      --
      aVariables('UNDCOASECONT') := f_unidad_get(pn_IDRemanente_i => F_IDREMANENTE_GET('TC'
                                                                                      ,Reg_Tramites.Ntramite));
      n_Tasa := f_ttipcamb_get(aVariables('UNDCOASECONT')
                              ,d_FeOcurrencia);
      aVariables('COASEUNCONT') := TO_NUMBER(aVariables('COASEGUROAPLICADO')) *
                                   n_Tasa; --Coa Aplicado en moneda contratada
      --
      aVariables('CDESTADO') := F_ESTADO_GET(aMesaControl(1).estatus);
      aVariables('CDESTADOPAG') := f_tmanteni_get_desc('TSTPAGSIN'
                                                      ,aVariables('CDESTADOPAG'));
      aVariables('MODALIDAD') := f_tmanteni_get_desc('MOD_SINI'
                                                    ,aMesaControl(1).OtValor06);
      aVariables('CAUSASINIEST') := f_tmanteni_get_desc('0ORIGSINI'
                                                       ,aIcd(1).cdorigen);
      IF aVariables('UNDPAGOCONT') IS NOT NULL AND
         aVariables('IMPPAGOCONT') > 0 THEN
        i := i + 1;
        aPagAprob.extend;
        aPagAprob(i) := T_TRAM_PAG_APROB(NTRAMITE     => Reg_Tramites.Ntramite
                                        ,TRAMITE      => aMesaControl(1).Otvalor15
                                        ,CDESTADO     => aVariables('CDESTADO')
                                        ,CDESTADOPAG  => aVariables('CDESTADOPAG')
                                        ,NMPOLIZA     => aPoliza(1).NmPoliex
                                        ,FECHATRAMITE => aMesaControl(1).ferecepc
                                        ,MODALIDAD    => aVariables('MODALIDAD')
                                        ,CAUSASINIEST => aVariables('CAUSASINIEST')
                                        ,PTCOASEGURO  => round(aVariables('COASEUNCONT')
                                                              ,2)
                                        ,PTDEDUCIBLE  => aVariables('DEDUUNCONT')
                                        ,IMPPAGOPESO  => round(aVariables('IMPPAGOPESO')
                                                              ,2)
                                        ,IMPPAGOCONT  => round(aVariables('IMPPAGOCONT')
                                                              ,2)
                                        ,UNDPAGOCONT  => aVariables('UNDPAGOCONT')
                                        ,DEDUUNCONT   => round(aVariables('DEDUCIBLEAPLICADO')
                                                              ,2)
                                        ,UNDDEDUCCONT => aVariables('UNDDEDUCCONT')
                                        ,COASEUNCONT  => ROUND(aVariables('COASEGUROAPLICADO')
                                                              ,2)
                                        ,UNDCOASECONT => aVariables('UNDCOASECONT')
                                         --,MONEDAPAGO   => aVariables('MONEDAPAGO')
                                        ,MONEDAPAGO => aVariables('UNDDEDUCCONT'));
      END IF;
    END LOOP;
    CLOSE cur_tramites;
    OPEN psrc_Data_o FOR
      SELECT * FROM TABLE(aPagAprob);
    RETURN psrc_Data_o;
  END;

  FUNCTION F_MODIFICAR_INICIAL(pn_ntramite_i IN tmesacontrol.ntramite%TYPE
                              ,pv_CdConval_i IN TVALOREMADET.cdconval%TYPE
                              ,pn_monto_i    IN NUMBER
                              ,pv_unidad_i   IN VARCHAR2) RETURN VARCHAR2 IS
    n_Suma     NUMBER;
    n_NmOrdina PLS_INTEGER;
    CURSOR Cur_R IS
      SELECT idRemanente
        FROM TVALOREMA R
       WHERE EXISTS
       (SELECT NULL
                FROM TVALOREMADET D
               WHERE D.IDREMANENTE = R.IDREMANENTE
                 AND D.NTRAMITE = pn_ntramite_i
                 AND (d.cdconval = pv_CdConval_i OR pv_CdConval_i IS NULL));
    Reg_R Cur_R%ROWTYPE;
    CURSOR Cur_D IS
      SELECT SUM(D.OTVALOR01) AS contratado
        FROM tvaloremadet d
       WHERE d.idremanente = reg_r.idremanente
         AND d.status = 'R';
    Reg_D    Cur_D%ROWTYPE;
    aValores t_100_30;
  BEGIN
    LOAD_DATOS(pn_ntramite_i);
    dbms_output.put_line(' entro ');
    OPEN Cur_R;
    LOOP
      FETCH Cur_R
        INTO Reg_R;
      EXIT WHEN Cur_R%NOTFOUND;
      n_NmOrdina := F_MAX_NUMORD(reg_r.idremanente
                                ,'R');
      dbms_output.put_line(' n_NmOrdina: ' || n_NmOrdina);
      OPEN cur_d;
      LOOP
        FETCH cur_d
          INTO reg_d;
        EXIT WHEN cur_D%NOTFOUND;
        dbms_output.put_line('reg_d.contratado:    ' || reg_d.contratado);
        dbms_output.put_line('pn_monto_i:          ' || pn_monto_i);
        dbms_output.put_line('pv_unidad_i:         ' || pv_unidad_i);
        aValores('OTVALOR01') := pn_monto_i - nvl(reg_d.contratado
                                                 ,0);
        aValores('OTVALOR02') := NVL(f_unidad_get(Reg_R.Idremanente)
                                    ,pv_unidad_i);
        IF aValores('OTVALOR01') != 0 THEN
          n_NmOrdina := F_MAX_NUMORD(pn_IdRemanente_i => Reg_R.Idremanente) + 1;
          P_TVALOREMADET_INS(pn_IdRemanente_i => Reg_R.Idremanente
                            ,pn_NmOrdina_i    => n_NmOrdina
                            ,pv_CdConval_i    => pv_CdConval_i
                            ,pv_Status_i      => 'R'
                            ,pd_FESTATUS_i    => SYSDATE
                            ,pv_OtValor01_i   => aValores('OTVALOR01')
                            ,pv_OtValor02_i   => aValores('OTVALOR02'));
          --Sumamos los Registrados - los pagados
          n_Suma := F_SUM_OTVALOR01_GET(pn_IdRemanente_i => Reg_R.Idremanente
                                       ,pv_Status_i      => 'R') -
                    F_SUM_OTVALOR01_GET(pn_IdRemanente_i => Reg_R.Idremanente
                                       ,pv_Status_i      => 'P');
          --Actualizamos el Remanente con la suma del detalle
          P_TVALOREMA_UPD(pn_IdRemanente_i => Reg_R.Idremanente
                         ,pv_OtValor01_i   => n_Suma);
        END IF;
      END LOOP;
      CLOSE cur_D;
    END LOOP;
    CLOSE Cur_R;
    RETURN 'ORDS-1002';
  EXCEPTION
    WHEN OTHERS THEN
      Pkg_Traduc.p_inserta_bitacora(pi_msg_id   => 1
                                   ,pi_code_err => SQLCODE
                                   ,pi_msg_text => SQLERRM
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'PKG_REMANENTE.F_REACTIVAR'
                                   ,pi_tipo     => 'Error');
      RETURN 'ORDS-2001';
  END;

  FUNCTION f_tvaloremadet_get(pn_ntramite_i IN ice.tmesacontrol.otvalor15 %TYPE
                             ,pn_start_io   IN OUT NUMBER
                             ,pn_limit_io   IN OUT NUMBER
                             ,pn_count_o    OUT NUMBER) RETURN SYS_REFCURSOR IS
    -- --                                  
    ps_salida_o SYS_REFCURSOR;
    pn_msg_id_o gb_messages.msg_id%TYPE := NULL;
    pv_title_o  gb_messages.title %TYPE := NULL;
    -- --   
  BEGIN
    LOAD_DATOS(pn_ntramite_i => pn_ntramite_i);
    pn_start_io := NVL(pn_start_io
                      ,0);
    pn_limit_io := NVL(pn_limit_io
                      ,999999);
    BEGIN
      OPEN ps_salida_o FOR
        SELECT tme.otvalor15 AS nro_tramite
              ,tme.ntramite AS tramite_interno
              ,tva.festatus AS Fecha_Movimiento
              ,tva.nmordina AS num_movimiento
              ,tma.descripc AS tipo_movimiento
              ,CASE
                 WHEN tco.cdtipcon IN ('HMED'
                                      ,'GHOSP'
                                      ,'GFHOS') THEN
                  'Suma Asegurada'
                 ELSE
                  tco.dsconval
               END AS reglon
              ,CASE
                 WHEN tco.cdtipcon IN ('DIGHO'
                                      ,'DNGHO'
                                      ,'GHOSP'
                                      ,'GFHOS') THEN
                  to_char(nvl(tva.otvalor01
                             ,'0.00')
                         ,'999,999,999,999.99') || ' ' || tva.otvalor02
               END AS "GS"
               ,CASE
                 WHEN tco.cdtipcon IN ('HMED'
                                      ,'TPCO') THEN
                  to_char(nvl(tva.otvalor01
                             ,'0.00')
                         ,'999,999,999,999.99') || ' ' || tva.otvalor02
               END AS "HS"
          FROM tvaloremadet     tva
              ,ice.tmesacontrol tme
              ,tconcsin         tco
              ,ice.tmanteni     tma
         WHERE tva.ntramite = pn_ntramite_i
           AND tme.ntramite = tva.ntramite
           AND tco.cdconval = tva.cdconval
           AND tva.otvalor01 IS NOT NULL
           AND to_number(tva.OTVALOR01) != 0
           AND tma.cdtabla = 'REMDESTS'
           AND tma.codigo = tva.status
           AND EXISTS
         (SELECT NULL
                  FROM tconfrema tcr
                      ,tvalorema tvr
                 WHERE tcr.cdtiporema IN ('SA'
                                         ,'DE'
                                         ,'DI'
                                         ,'TC')
                   AND TCR.SWACTIVO = K_S
                   AND tcr.cdramo = aMesaControl(1).CdRamo
                   AND tcr.cdnivel = tvr.cdnivel
                   AND tcr.cdconval = tco.cdconval
                   AND tcr.cdremanente = tvr.cdremanente
                   AND tvr.idremanente = tva.idremanente)
         GROUP BY tme.otvalor15
                 ,tme.ntramite
                 ,tva.festatus
                 ,tva.nmordina
                 ,tma.descripc
                 ,tco.cdtipcon
                 ,tco.dsconval
                 ,tva.otvalor01
                 ,tva.otvalor02
         ORDER BY 2
                 ,4
                 ,6 ASC offset pn_start_io rows
         FETCH NEXT pn_limit_io rows ONLY;
    END;
    BEGIN
      SELECT COUNT(*) conta
        INTO pn_count_o
        FROM (SELECT tme.otvalor15
                    ,tme.ntramite
                    ,tva.festatus
                    ,tva.nmordina
                    ,tma.descripc
                    ,tco.cdtipcon
                    ,tco.dsconval
                    ,tva.otvalor01
                    ,tva.otvalor02
                FROM tvaloremadet     tva
                    ,ice.tmesacontrol tme
                    ,tconcsin         tco
                    ,ice.tmanteni     tma
               WHERE tva.ntramite = pn_ntramite_i
                 AND tme.ntramite = tva.ntramite
                 AND tco.cdconval = tva.cdconval
                 AND tva.otvalor01 IS NOT NULL
                 AND to_number(tva.OTVALOR01) != 0
                 AND tma.cdtabla = 'REMDESTS'
                 AND tma.codigo = tva.status
                 AND EXISTS
               (SELECT NULL
                        FROM tconfrema tcr
                            ,tvalorema tvr
                       WHERE tcr.cdtiporema IN ('SA'
                                               ,'DE'
                                               ,'DI'
                                               ,'TC')
                         AND TCR.SWACTIVO = K_S
                         AND tcr.cdramo = aMesaControl(1).CdRamo
                         AND tcr.cdnivel = tvr.cdnivel
                         AND tcr.cdconval = tco.cdconval
                         AND tcr.cdremanente = tvr.cdremanente
                         AND tvr.idremanente = tva.idremanente)
               GROUP BY tme.otvalor15
                       ,tme.ntramite
                       ,tva.festatus
                       ,tva.nmordina
                       ,tma.descripc
                       ,tco.cdtipcon
                       ,tco.dsconval
                       ,tva.otvalor01
                       ,tva.otvalor02) dat;
      RETURN ps_salida_o;
    END;
    -- --
  EXCEPTION
    WHEN OTHERS THEN
      pn_msg_id_o := 100000;
      pv_title_o  := SUBSTR(100000
                           ,1
                           ,1);
      pkg_traduc.p_inserta_bitacora(pi_msg_id   => pn_msg_id_o
                                   ,pi_code_err => DBMS_UTILITY.format_error_backtrace
                                   ,pi_msg_text => DBMS_UTILITY.FORMAT_ERROR_STACK
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'f_tvaloremadet_get'
                                   ,pi_dsaccion => 'Error '
                                   ,pi_tipo     => pv_title_o);
      RETURN NULL;
  END f_tvaloremadet_get;

  FUNCTION f_modificar_inicial_post(pn_ntramite_i IN ice.tmesacontrol.otvalor15 %TYPE)
    RETURN VARCHAR2 IS
    v_sql       VARCHAR2(4000);
    v_Return    VARCHAR2(50) := 'ORDS-1002';
    pn_monto    NUMBER;
    pv_unidad   VARCHAR2(10);
    pn_msg_id_o gb_messages.msg_id%TYPE := NULL;
    pv_title_o  gb_messages.title %TYPE := NULL;
    CURSOR cur_dat IS
      SELECT tco.cdtipcon
            ,tco.cdconval
            ,tta.otvalor01
            ,tta.otvalor02
        FROM tsinigar tsi
            ,tragacov tra
            ,tconcsin tco
            ,ttapvaat tta
       WHERE tsi.ntramite = pn_ntramite_i
         AND tsi.cdunieco = amesacontrol(1).cdunieco
         AND tsi.cdramo = amesacontrol(1).cdramo
         AND tsi.aaapertu = asiniestro(1).aaapertu
         AND tsi.nmsinies = asiniestro(1).nmsinies
         AND tsi.cdramo = tra.cdramo
         AND tsi.cdgarant = tra.cdgarant
         AND tra.cdconval = tco.cdconval
         AND tta.nmtabla IN
             (SELECT tt.nmtabla
                FROM ttaptabl tt
               WHERE tt.cdtabla IN ('TCONCSINCO'))
         AND tsi.cdtipo = tta.otclave1
         AND tco.cdtipcon = tta.otclave2;
    reg_dat cur_dat%ROWTYPE;
  BEGIN
    -- --
    load_datos(pn_ntramite_i);
    -- --
    OPEN cur_dat;
    LOOP
      FETCH cur_dat
        INTO reg_dat;
      EXIT WHEN cur_dat%NOTFOUND;
      BEGIN
        v_sql := ' select tec.' || reg_dat.otvalor01 || ',tec.' ||
                 reg_dat.otvalor02 || '  from  testcond tec' ||
                 ' where tec.ntramite   = :1 ' ||
                 '   and tec.cdunieco   = :2 ' ||
                 '   and tec.cdramo     = :3 ' ||
                 '   and tec.aaapertu   = :4 ' ||
                 '   and tec.nmsinies   = :5 ' ||
                 '   and tec.cdtipmix   = :6 ' ||
                 '   and tec.nmordina   = (select max(tes.nmordina)
                                           from testcond tes
                                           where tes.cdunieco = tec.cdunieco
                                             and tes.ntramite = tec.ntramite
                                             and tes.cdramo   = tec.cdramo
                                             and tes.aaapertu = tec.aaapertu
                                             and tes.nmsinies = tec.nmsinies
                                             and tes.cdtipmix = tec.cdtipmix)';
        dbms_output.put_line('v_sql: ' || v_sql);
        EXECUTE IMMEDIATE v_sql
          INTO pn_monto, pv_unidad
          USING amesacontrol(1).ntramite, amesacontrol(1).cdunieco, amesacontrol(1).cdramo, asiniestro(1).aaapertu, asiniestro(1).nmsinies, 'CR';
      EXCEPTION
        WHEN no_data_found THEN
          pn_monto := NULL;
      END;
      -- --
      IF nvl(pn_monto
            ,0) > 0 THEN
        v_Return := pkg_remanente.f_modificar_inicial(pn_ntramite_i => pn_ntramite_i
                                                     ,pv_cdconval_i => reg_dat.cdconval
                                                     ,pn_monto_i    => pn_monto
                                                     ,pv_unidad_i   => pv_unidad);
      END IF;
    END LOOP;
    CLOSE cur_dat;
    RETURN v_return;
  EXCEPTION
    WHEN OTHERS THEN
      pn_msg_id_o := 100000;
      pv_title_o  := SUBSTR(100000
                           ,1
                           ,1);
      pkg_traduc.p_inserta_bitacora(pi_msg_id   => pn_msg_id_o
                                   ,pi_code_err => DBMS_UTILITY.format_error_backtrace
                                   ,pi_msg_text => DBMS_UTILITY.FORMAT_ERROR_STACK
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'f_modificar_inicial_post'
                                   ,pi_dsaccion => 'Error '
                                   ,pi_tipo     => pv_title_o);
      RETURN 'ORDS-1003';
  END f_modificar_inicial_post;

  -- --
  FUNCTION F_FINIQUITO_GET(pn_ntramite_i IN tmesacontrol.ntramite%TYPE)
    RETURN tab_vardat IS
    aFiniquito  t_tab_aFiniquito;
    i           PLS_INTEGER := 0;
    pv_cdtipo_i VARCHAR2(200) := 'P';
    cc_mnd CONSTANT VARCHAR2(100) := 'MXN';
    pv_Dummy         VARCHAR2(5) := NULL;
    pv_tipo          VARCHAR2(500) := NULL;
    v_DsConcepto     VARCHAR2(500) := NULL;
    pv_Unidad        VARCHAR2(5) := NULL;
    pv_UnidadC       VARCHAR2(5) := NULL;
    n_contratado     NUMBER := NULL;
    n_satisfecho     NUMBER := NULL;
    n_saldos         NUMBER := NULL;
    n_montos         NUMBER := NULL;
    n_tramite        NUMBER := NULL;
    n_siniestro      NUMBER := NULL;
    pn_sumnmsumarem  NUMBER := NULL;
    pn_sumnmsumactra NUMBER := NULL;
    pn_sumnmsumapag  NUMBER := NULL;
    pn_sumnmdedurem  NUMBER := NULL;
    pn_sumnmdeductra NUMBER := NULL;
    pn_sumnmdedupag  NUMBER := NULL;
    pn_sumnmcoasrem  NUMBER := NULL;
    pn_sumnmcoasctra NUMBER := NULL;
    pn_sumnmcoaspag  NUMBER := NULL;
    n_nmordina       NUMBER;
    d_FeOcurrencia   DATE := PKG_REGLAS_CAL.F_OCURRENCIA_TRAMITE_GET(pn_ntramite_i => pn_ntramite_i);
    -- --
    l_data tab_vardat := tab_vardat();
    -- --
    CURSOR c_tab_act IS
      SELECT vardat_rectyp(orden
                          ,tipo
                          ,etiqueta
                          ,contratado
                          ,satisfecho
                          ,saldos
                          ,montos
                          ,tramite
                          ,siniestro
                          ,moneda
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL
                          ,NULL)
        FROM (SELECT * FROM TABLE(afiniquito) WHERE ntramite = pn_ntramite_i)
       ORDER BY orden;
    -- --
    PROCEDURE P_CONCEPTO_SET(pn_ntramite_i  IN NUMBER
                            ,pv_tipo_i      IN VARCHAR2
                            ,v_DsConcepto_i IN VARCHAR2
                            ,n_contratado_i IN NUMBER
                            ,n_satisfecho_i IN NUMBER
                            ,n_saldos_i     IN NUMBER
                            ,n_montos_i     IN NUMBER
                            ,n_tramite_i    IN NUMBER
                            ,n_siniestro_i  IN NUMBER
                            ,pv_Unidad_i    IN VARCHAR2) IS
    BEGIN
      -- --
      i := i + 1;
      aFiniquito.extend;
      aFiniquito(i) := T_FINIQUITO(ntramite   => pn_ntramite_i
                                  ,orden      => i
                                  ,tipo       => pv_tipo_i
                                  ,etiqueta   => InitCap(v_DsConcepto_i)
                                  ,contratado => ROUND(n_contratado_i
                                                      ,2)
                                  ,satisfecho => ROUND(n_satisfecho_i
                                                      ,2)
                                  ,saldos     => ROUND(n_saldos_i
                                                      ,2)
                                  ,montos     => ROUND(n_montos_i
                                                      ,2)
                                  ,tramite    => ROUND(n_tramite_i
                                                      ,2)
                                  ,siniestro  => ROUND(n_siniestro_i
                                                      ,2)
                                  ,moneda     => pv_Unidad_i);
    END;
  
    -- --
  BEGIN
    LOAD_DATOS(pn_ntramite_i => pn_ntramite_i);
    aFiniquito       := t_TAB_aFiniquito();
    pv_tipo          := 'Condiciones Contratadas / Remanente';
    pn_sumnmsumactra := PKG_REMANENTE.F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                                     ,pv_CdNivel_i     => 'PA'
                                                     ,pv_cdtiporema_i  => 'SA'
                                                     ,pv_PorTramite_i  => 'N'
                                                     ,pv_Status_i      => 'R'
                                                     ,pv_cdtipcon_i    => NULL
                                                     ,pv_unidad_o      => pv_UnidadC
                                                     ,pv_nmordina_o    => n_nmordina
                                                     ,pn_cdremanente_i => NULL
                                                     ,pv_cdtipo_i      => pv_cdtipo_i);
    pn_sumnmsumapag  := PKG_REMANENTE.F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                                     ,pv_CdNivel_i     => 'PA'
                                                     ,pv_cdtiporema_i  => 'SA'
                                                     ,pv_PorTramite_i  => 'N'
                                                     ,pv_Status_i      => 'P'
                                                     ,pv_cdtipcon_i    => NULL
                                                     ,pv_unidad_o      => pv_Dummy
                                                     ,pv_nmordina_o    => n_nmordina
                                                     ,pn_cdremanente_i => NULL
                                                     ,pv_cdtipo_i      => pv_cdtipo_i);
    pn_sumnmsumarem  := pkg_remanente.f_remanente_final_get(pn_ntramite_i           => pn_ntramite_i
                                                           ,pv_cdnivel_i            => 'PA'
                                                           ,pv_cdtiporema_i         => 'SA'
                                                           ,pv_cdtipcon_i           => NULL
                                                           ,pv_sumartramiteactual_i => 'N'
                                                           ,pv_unidad_o             => pv_Dummy);
    v_DsConcepto     := 'Suma Asegurada';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => pn_sumnmsumactra
                  ,n_satisfecho_i => pn_sumnmsumapag
                  ,n_saldos_i     => pn_sumnmsumarem
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_UnidadC);
    -- --
    pn_sumnmdeductra := PKG_REMANENTE.F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                                     ,pv_CdNivel_i     => 'PA'
                                                     ,pv_cdtiporema_i  => 'DE'
                                                     ,pv_PorTramite_i  => 'N'
                                                     ,pv_Status_i      => 'R'
                                                     ,pv_cdtipcon_i    => NULL
                                                     ,pv_unidad_o      => pv_UnidadC
                                                     ,pv_nmordina_o    => n_nmordina
                                                     ,pn_cdremanente_i => NULL
                                                     ,pv_cdtipo_i      => pv_cdtipo_i);
    pn_sumnmdedupag  := PKG_REMANENTE.F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                                     ,pv_CdNivel_i     => 'PA'
                                                     ,pv_cdtiporema_i  => 'DE'
                                                     ,pv_PorTramite_i  => 'N'
                                                     ,pv_Status_i      => 'P'
                                                     ,pv_cdtipcon_i    => NULL
                                                     ,pv_unidad_o      => pv_Dummy
                                                     ,pv_nmordina_o    => n_nmordina
                                                     ,pn_cdremanente_i => NULL
                                                     ,pv_cdtipo_i      => pv_cdtipo_i);
    pn_sumnmdedurem  := pkg_remanente.f_remanente_final_get(pn_ntramite_i           => pn_ntramite_i
                                                           ,pv_cdnivel_i            => 'PA'
                                                           ,pv_cdtiporema_i         => 'DE'
                                                           ,pv_cdtipcon_i           => NULL
                                                           ,pv_sumartramiteactual_i => 'N'
                                                           ,pv_unidad_o             => pv_Dummy);
    v_DsConcepto     := 'Deducible';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => pn_sumnmdeductra
                  ,n_satisfecho_i => pn_sumnmdedupag
                  ,n_saldos_i     => pn_sumnmdedurem
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_UnidadC);
    -- --
    pn_sumnmcoasctra := PKG_REMANENTE.F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                                     ,pv_CdNivel_i     => 'PA'
                                                     ,pv_cdtiporema_i  => 'TC'
                                                     ,pv_PorTramite_i  => 'N'
                                                     ,pv_Status_i      => 'R'
                                                     ,pv_cdtipcon_i    => NULL
                                                     ,pv_unidad_o      => pv_UnidadC
                                                     ,pv_nmordina_o    => n_nmordina
                                                     ,pn_cdremanente_i => NULL
                                                     ,pv_cdtipo_i      => pv_cdtipo_i);
    pn_sumnmcoaspag  := PKG_REMANENTE.F_REMANENTE_GET(pn_ntramite_i    => pn_ntramite_i
                                                     ,pv_CdNivel_i     => 'PA'
                                                     ,pv_cdtiporema_i  => 'TC'
                                                     ,pv_PorTramite_i  => 'N'
                                                     ,pv_Status_i      => 'P'
                                                     ,pv_cdtipcon_i    => NULL
                                                     ,pv_unidad_o      => pv_Dummy
                                                     ,pv_nmordina_o    => n_nmordina
                                                     ,pn_cdremanente_i => NULL
                                                     ,pv_cdtipo_i      => pv_cdtipo_i);
    pn_sumnmcoasrem  := pkg_remanente.f_remanente_final_get(pn_ntramite_i           => pn_ntramite_i
                                                           ,pv_cdnivel_i            => 'PA'
                                                           ,pv_cdtiporema_i         => 'TC'
                                                           ,pv_cdtipcon_i           => NULL
                                                           ,pv_sumartramiteactual_i => 'N'
                                                           ,pv_unidad_o             => pv_Dummy);
    v_DsConcepto     := 'Tope de Coaseguro';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => pn_sumnmcoasctra
                  ,n_satisfecho_i => pn_sumnmcoaspag
                  ,n_saldos_i     => pn_sumnmcoasrem
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_UnidadC);
    -- --
    n_contratado := pkg_siniestro_func.f_tasa_tdetimptra(pn_ntramite_i
                                                        ,4);
    pv_Unidad    := '%';
    v_DsConcepto := 'Coaseguro';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    ---------------------------------------------------
    n_contratado := NULL;
    n_satisfecho := NULL;
    n_saldos     := NULL;
    n_montos     := NULL;
    n_tramite    := NULL;
    n_siniestro  := NULL;
    pv_Unidad    := NULL;
    ---------------------------------------------------
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,18);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,18);
    pv_tipo      := 'Resumen de Gastos';
    v_DsConcepto := 'Total(tramite)';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,27);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,27);
    v_DsConcepto := 'Gastos no Cubiertos';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,18);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,18);
    v_DsConcepto := 'Monto Procedente';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,29);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,29);
    v_DsConcepto := 'Deducible';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,7);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,7);
    v_DsConcepto := 'Coaseguro';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,19);
    n_montos     := (n_montos +
                    pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                        ,20));
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,20);
    v_DsConcepto := 'Participación Adicional';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    n_montos     := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,12);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,12);
    v_DsConcepto := 'Total a Pagar';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => pv_Unidad);
    ---------------------------------------------------
    n_contratado := NULL;
    n_satisfecho := NULL;
    n_saldos     := NULL;
    n_montos     := NULL;
    n_tramite    := NULL;
    n_siniestro  := NULL;
    pv_Unidad    := NULL;
    ---------------------------------------------------
    pv_tipo      := 'Saldo Actual';
    v_DsConcepto := 'Suma Asegurada (Saldo)';
    n_tramite    := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,12);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,12);
    n_tramite    := pkg_siniestro_cal.f_ttipcamb(n_tramite
                                                ,pv_Unidad
                                                ,cc_mnd
                                                ,d_feocurrencia);
    n_siniestro  := pkg_remanente.f_remanente_final_get(pn_ntramite_i           => pn_ntramite_i
                                                       ,pv_cdnivel_i            => 'PA'
                                                       ,pv_cdtiporema_i         => 'SA'
                                                       ,pv_cdtipcon_i           => NULL
                                                       ,pv_sumartramiteactual_i => 'S'
                                                       ,pv_unidad_o             => pv_Dummy);
    n_siniestro  := pkg_siniestro_cal.f_ttipcamb(n_siniestro
                                                ,pv_Dummy
                                                ,cc_mnd
                                                ,d_feocurrencia);
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => cc_mnd);
    n_tramite    := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,29);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,29);
    n_tramite    := pkg_siniestro_cal.f_ttipcamb(n_tramite
                                                ,pv_Unidad
                                                ,cc_mnd
                                                ,d_feocurrencia);
    n_siniestro  := pkg_remanente.f_remanente_final_get(pn_ntramite_i           => pn_ntramite_i
                                                       ,pv_cdnivel_i            => 'PA'
                                                       ,pv_cdtiporema_i         => 'DE'
                                                       ,pv_cdtipcon_i           => NULL
                                                       ,pv_sumartramiteactual_i => 'S'
                                                       ,pv_unidad_o             => pv_Dummy);
    n_siniestro  := pkg_siniestro_cal.f_ttipcamb(n_siniestro
                                                ,pv_Dummy
                                                ,cc_mnd
                                                ,d_feocurrencia);
    v_DsConcepto := 'Deducible (Saldo)';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => cc_mnd);
    n_tramite    := pkg_siniestro_func.f_mto_tdetimptra(pn_ntramite_i
                                                       ,7);
    pv_Unidad    := pkg_siniestro_func.f_mon_tdetimptra(pn_ntramite_i
                                                       ,7);
    n_tramite    := pkg_siniestro_cal.f_ttipcamb(n_tramite
                                                ,pv_Unidad
                                                ,cc_mnd
                                                ,d_feocurrencia);
    n_siniestro  := pkg_remanente.f_remanente_final_get(pn_ntramite_i           => pn_ntramite_i
                                                       ,pv_cdnivel_i            => 'PA'
                                                       ,pv_cdtiporema_i         => 'TC'
                                                       ,pv_cdtipcon_i           => NULL
                                                       ,pv_sumartramiteactual_i => 'S'
                                                       ,pv_unidad_o             => pv_Dummy);
    n_siniestro  := pkg_siniestro_cal.f_ttipcamb(n_siniestro
                                                ,pv_Dummy
                                                ,cc_mnd
                                                ,d_feocurrencia);
    v_DsConcepto := 'Tope de Coaseguro (Saldo)';
    P_CONCEPTO_SET(pn_ntramite_i  => pn_ntramite_i
                  ,pv_tipo_i      => pv_tipo
                  ,v_DsConcepto_i => v_DsConcepto
                  ,n_contratado_i => n_contratado
                  ,n_satisfecho_i => n_satisfecho
                  ,n_saldos_i     => n_saldos
                  ,n_montos_i     => n_montos
                  ,n_tramite_i    => n_tramite
                  ,n_siniestro_i  => n_siniestro
                  ,pv_Unidad_i    => cc_mnd);
    OPEN c_tab_act;
    LOOP
      l_data.extend;
      FETCH c_tab_act BULK COLLECT
        INTO l_data;
      EXIT WHEN c_tab_act%NOTFOUND;
    END LOOP;
    -- --
    RETURN l_data;
    -- --
  END;

  FUNCTION F_ACUMULADOS_COB_GET(pn_CdUnieco_i IN MSINIEST.CDUNIECO%TYPE
                               ,pn_CdRamo_i   IN MSINIEST.CDRAMO%TYPE
                               ,pn_aaapertu_i IN MSINIEST.AAAPERTU%TYPE
                               ,pn_nmsinies_i IN MSINIEST.NMSINIES%TYPE
                               ,pv_CdGarant_i IN TSINIGAR.CDGARANT%TYPE
                               ,pv_CdCodIcd_i IN MSINIICD.CDCOICD%TYPE)
    RETURN SYS_REFCURSOR IS
    aAcumulados         t_tab_acumulados;
    i                   PLS_INTEGER := 0;
    v_DsConcepto        VARCHAR2(100);
    n_ImporteC          NUMBER;
    n_ImporteT          NUMBER;
    n_ImporteR          NUMBER;
    v_Unidad            VARCHAR2(15);
    v_UnidadC           VARCHAR2(15);
    v_Dummy             VARCHAR2(15);
    psrc_Data_o         SYS_REFCURSOR;
    n_ImporteP          NUMBER;
    v_Encabezado        VARCHAR2(200);
    d_FeOcurrencia      DATE; --:= PKG_REGLAS_CAL.F_OCURRENCIA_TRAMITE_GET(pn_ntramite_i => pn_ntramite_i);
    n_ImporteConvertido NUMBER;
    n_ImporteExceso     NUMBER := 0;
    n_nmordina          NUMBER;
    --
    PROCEDURE P_PLANCHADO_VARIABLES IS
    BEGIN
      aVariables('CDUNIECO') := pn_CdUnieco_i;
      aVariables('CDRAMO') := pn_cdramo_i;
      aVariables('CDGARANT') := pv_CdGarant_i;
      aVariables('CDICD') := pv_CdCodIcd_i;
      aVariables('AAAPERTU') := pn_Aaapertu_i;
      aVariables('NMSINIES') := pn_NMSINIES_i;
      b_VariablesPlanchadas := TRUE;
    END;
  
    --
    PROCEDURE P_CONCEPTO_SET(pv_TipoRema_i   IN VARCHAR2
                            ,pv_CdNivel_i    IN VARCHAR2
                            ,pv_Status_i     IN VARCHAR2 DEFAULT 'P'
                            ,pv_PorTramite_i IN VARCHAR2 DEFAULT 'N'
                            ,pv_CdTipCon_i   IN VARCHAR2
                            ,pv_Tipo_i       IN VARCHAR2) IS
      FUNCTION F_TCONCSIN_DESC_GET(pv_cdtipcon_i IN TCONCSIN.cdtipcon%TYPE)
        RETURN VARCHAR2 IS
        CURSOR Cur_DsCpto IS
          SELECT t.dsconval
            FROM TCONCSIN t
           WHERE t.cdtipcon = pv_cdtipcon_i;
        v_DsConcepto TCONCSIN.DSCONVAL%TYPE;
      BEGIN
        OPEN Cur_DsCpto;
        FETCH Cur_DsCpto
          INTO v_DsConcepto;
        CLOSE Cur_DsCpto;
        RETURN v_DsConcepto;
      END;
    
    BEGIN
      IF pv_CdTipCon_i IS NOT NULL THEN
        v_DsConcepto := F_TCONCSIN_DESC_GET(pv_CdTipCon_i);
      ELSE
        v_DsConcepto := F_TMANTENI_GET_DESC('TIPOREMA'
                                           ,pv_TipoRema_i);
      END IF;
      --
      v_DsConcepto := InitCap(v_DsConcepto);
      BEGIN
        IF pv_Status_i != 'T' THEN
          n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => NULL
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => 'N'
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_UnidadC
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          --
          n_ImporteC := NVL(n_ImporteC
                           ,0);
          BEGIN
            n_ImporteConvertido := n_ImporteC *
                                   f_ttipcamb_get(pv_cdmoneda_i => v_Unidad
                                                 ,pv_fecambio_i => d_FeOcurrencia);
          EXCEPTION
            WHEN OTHERS THEN
              n_ImporteConvertido := 0;
          END;
          -- -- 
          n_ImporteP := F_REMANENTE_GET(pn_ntramite_i    => NULL
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => pv_PorTramite_i
                                       ,pv_Status_i      => pv_Status_i
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Dummy
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          n_importeP := round(NVL(n_importeP
                                 ,0)
                             ,2);
          -- --
          n_ImporteR := round(n_ImporteC - n_importeP
                             ,2);
          i          := i + 1;
          aAcumulados.extend;
          aAcumulados(i) := T_ACUMULADOS(orden      => i
                                        ,etiqueta   => v_DsConcepto
                                        ,contratado => n_ImporteC
                                        ,moneda     => v_Unidad
                                        ,otsimbol   => v_Unidad
                                        ,pagado     => n_ImporteP
                                        ,porcentaje => NULL
                                        ,remanente  => n_ImporteR
                                        ,tipo       => pv_tipo_i
                                        ,convertido => n_ImporteConvertido
                                        ,exceso     => n_ImporteExceso);
        ELSE
          n_ImporteC := F_REMANENTE_GET(pn_ntramite_i    => NULL
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => pv_PorTramite_i
                                       ,pv_Status_i      => 'R'
                                       ,pv_cdtipcon_i    => NULL
                                       ,pv_Unidad_o      => v_UnidadC
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          n_ImporteT := F_REMANENTE_GET(pn_ntramite_i    => NULL
                                       ,pv_CdNivel_i     => pv_CdNivel_i
                                       ,pv_cdtiporema_i  => pv_TipoRema_i
                                       ,pv_PorTramite_i  => pv_PorTramite_i
                                       ,pv_Status_i      => pv_Status_i
                                       ,pv_cdtipcon_i    => pv_CdTipCon_i
                                       ,pv_Unidad_o      => v_Unidad
                                       ,pv_nmordina_o    => n_nmordina
                                       ,pn_cdRemanente_i => NULL);
          v_Unidad   := v_UnidadC;
          i          := i + 1;
          aAcumulados.extend;
          aAcumulados(i) := T_ACUMULADOS(orden      => i
                                        ,etiqueta   => v_DsConcepto
                                        ,contratado => ROUND(n_ImporteT
                                                            ,2)
                                        ,moneda     => v_Unidad
                                        ,otsimbol   => v_Unidad
                                        ,pagado     => ROUND(n_ImporteP
                                                            ,2)
                                        ,porcentaje => NULL
                                        ,remanente  => n_ImporteR
                                        ,tipo       => pv_tipo_i
                                        ,convertido => ROUND(n_ImporteConvertido
                                                            ,2)
                                        ,exceso     => ROUND(n_ImporteExceso
                                                            ,2));
        END IF;
      END;
    END;
  
    PROCEDURE P_ACUMULADOS_PORC IS
      CURSOR Cur_p IS
        SELECT nmcoanac
              ,nmcoaext
          FROM (SELECT T.nmcoanac
                      ,T.nmcoaext
                      ,RANK() OVER(PARTITION BY CDTIPMIX ORDER BY NMORDINA DESC) RANK_X
                  FROM TESTCOND T
                 WHERE T.ntramite = NULL
                   AND t.cdtipmix = 'CR')
         WHERE RANK_X = 1;
      Reg_P Cur_P%ROWTYPE;
    BEGIN
      OPEN cur_p;
      FETCH cur_p
        INTO reg_p;
      CLOSE cur_p;
      i := i + 1;
      aAcumulados.extend;
      aAcumulados(i) := T_ACUMULADOS(orden      => i
                                    ,etiqueta   => 'Coaseguro Nacional'
                                    ,contratado => NULL
                                    ,moneda     => NULL
                                    ,otsimbol   => NULL
                                    ,pagado     => NULL
                                    ,porcentaje => reg_p.nmcoanac
                                    ,remanente  => NULL
                                    ,tipo       => v_Encabezado
                                    ,convertido => n_ImporteConvertido
                                    ,exceso     => n_ImporteExceso);
      i := i + 1;
      aAcumulados.extend;
      aAcumulados(i) := T_ACUMULADOS(orden      => i
                                    ,etiqueta   => 'Coaseguro Extranjero'
                                    ,contratado => NULL
                                    ,moneda     => NULL
                                    ,otsimbol   => NULL
                                    ,pagado     => NULL
                                    ,porcentaje => reg_p.nmcoanac
                                    ,remanente  => NULL
                                    ,tipo       => v_Encabezado
                                    ,convertido => n_ImporteConvertido
                                    ,exceso     => n_ImporteExceso);
    END;
  
  BEGIN
    P_PLANCHADO_VARIABLES;
    LOAD_DATOS(pn_ntramite_i => NULL);
    aAcumulados  := t_TAB_Acumulados();
    v_Encabezado := 'Importe en Transito';
    P_CONCEPTO_SET(pv_TipoRema_i   => 'SA'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => 'GHOSP'
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'SA'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => 'HMED'
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DE'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DI'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'TC'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'T'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    --
    v_Encabezado := 'Acumulado';
    P_CONCEPTO_SET(pv_TipoRema_i   => 'SA'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DE'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'DI'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    P_CONCEPTO_SET(pv_TipoRema_i   => 'TC'
                  ,pv_CdNivel_i    => 'PA'
                  ,pv_Status_i     => 'P'
                  ,pv_PorTramite_i => 'N'
                  ,pv_CdTipCon_i   => NULL
                  ,pv_Tipo_i       => v_Encabezado);
    OPEN psrc_Data_o FOR
      SELECT * FROM TABLE(aAcumulados);
    RETURN psrc_Data_o;
  EXCEPTION
    WHEN OTHERS THEN
      pkg_traduc.p_inserta_bitacora(pi_msg_id   => 10000
                                   ,pi_code_err => SQLCODE
                                   ,pi_msg_text => v_Dummy
                                   ,pi_usuario  => USER
                                   ,pi_programa => 'PKG_REMANENTE.F_ACUMULADOS_COB_GET'
                                   ,pi_dsaccion => 'Error '
                                   ,pi_tipo     => 1);
      OPEN psrc_Data_o FOR
        SELECT NULL FROM DUAL;
      RETURN psrc_Data_o;
  END;

  FUNCTION F_TVALOREMADET_INS(pn_ntramite_i    IN tmesacontrol.ntramite%TYPE
                             ,pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                             ,pn_NmOrdina_i    IN TVALOREMADET.nmordina%TYPE
                             ,pv_CdConval_i    IN TVALOREMADET.cdconval%TYPE
                             ,pv_Status_i      IN TVALOREMADET.STATUS%TYPE
                             ,pd_FESTATUS_i    IN TVALOREMADET.FESTATUS%TYPE
                             ,pv_OtValor01_i   IN TVALOREMADET.otvalor01%TYPE
                             ,pv_OtValor02_i   IN TVALOREMADET.otvalor02%TYPE
                             ,pv_OtValor03_i   IN TVALOREMADET.otvalor03%TYPE DEFAULT NULL
                             ,pv_OtValor04_i   IN TVALOREMADET.otvalor04%TYPE DEFAULT NULL
                             ,pv_OtValor05_i   IN TVALOREMADET.otvalor05%TYPE DEFAULT NULL)
    RETURN VARCHAR2 IS
    v_Return VARCHAR2(30);
  BEGIN
    load_datos(pn_ntramite_i);
    P_TVALOREMADET_INS(pn_IdRemanente_i => pn_IdRemanente_i
                      ,pn_NmOrdina_i    => pn_NmOrdina_i
                      ,pv_CdConval_i    => pv_CdConval_i
                      ,pv_Status_i      => pv_Status_i
                      ,pd_FESTATUS_i    => NVL(pd_FESTATUS_i,SYSDATE)
                      ,pv_OtValor01_i   => pv_OtValor01_i
                      ,pv_OtValor02_i   => pv_OtValor02_i
                      ,pv_OtValor03_i   => pv_OtValor03_i
                      ,pv_OtValor04_i   => pv_OtValor04_i
                      ,pv_OtValor05_i   => pv_OtValor05_i);
    V_RETURN := 'ORDS-1003';
    RETURN V_RETURN;
  END;

  FUNCTION F_TVALOREMADET_UPD(pn_ntramite_i    IN tmesacontrol.ntramite%TYPE
                             ,pn_IdRemanente_i IN TVALOREMADET.IDREMANENTE%TYPE
                             ,pn_NmOrdina_i    IN TVALOREMADET.nmordina%TYPE
                             ,pv_CdConval_i    IN TVALOREMADET.cdconval%TYPE
                             ,pv_Status_i      IN TVALOREMADET.STATUS%TYPE
                             ,pd_FESTATUS_i    IN TVALOREMADET.FESTATUS%TYPE
                             ,pv_OtValor01_i   IN TVALOREMADET.otvalor01%TYPE
                             ,pv_OtValor02_i   IN TVALOREMADET.otvalor02%TYPE
                             ,pv_OtValor03_i   IN TVALOREMADET.otvalor03%TYPE DEFAULT NULL
                             ,pv_OtValor04_i   IN TVALOREMADET.otvalor04%TYPE DEFAULT NULL
                             ,pv_OtValor05_i   IN TVALOREMADET.otvalor05%TYPE DEFAULT NULL)
    RETURN VARCHAR2 IS
    v_Return VARCHAR2(30);
  BEGIN
    load_datos(pn_ntramite_i);
    
        P_TVALOREMADET_UPD(pn_IdRemanente_i => pn_IdRemanente_i
                          ,pn_NmOrdina_i    => pn_NmOrdina_i
                          ,pv_Status_i      => pv_Status_i
                          ,pd_FESTATUS_i    => pd_FESTATUS_i
                          ,pv_OtValor01_i   => pv_OtValor01_i
                          ,pv_OtValor02_i   => pv_OtValor02_i
                          ,pv_OtValor03_i   => pv_OtValor03_i
                          ,pv_OtValor04_i   => pv_OtValor04_i
                          ,pv_OtValor05_i   => pv_OtValor05_i);
    V_RETURN := 'ORDS-1003';
    RETURN V_RETURN;
  END;

-- --
END;
/
