(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  ISO3166                                                                   *)
(*  Ver.: 17.09.2019                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit ISO3166;

interface

type
  TISO3166_Alpha2code = (iso3166_A2C_none,
    iso3166_AF, iso3166_AX, iso3166_AL, iso3166_DZ, iso3166_AS, iso3166_AD,
    iso3166_AO, iso3166_AI, iso3166_AQ, iso3166_AG, iso3166_AR, iso3166_AM,
    iso3166_AW, iso3166_AU, iso3166_AT, iso3166_AZ, iso3166_BS, iso3166_BH,
    iso3166_BD, iso3166_BB, iso3166_BY, iso3166_BE, iso3166_BZ, iso3166_BJ,
    iso3166_BM, iso3166_BT, iso3166_BO, iso3166_BQ, iso3166_BA, iso3166_BW,
    iso3166_BV, iso3166_BR, iso3166_IO, iso3166_BN, iso3166_BG, iso3166_BF,
    iso3166_BI, iso3166_CV, iso3166_KH, iso3166_CM, iso3166_CA, iso3166_KY,
    iso3166_CF, iso3166_TD, iso3166_CL, iso3166_CN, iso3166_CX, iso3166_CC,
    iso3166_CO, iso3166_KM, iso3166_CD, iso3166_CG, iso3166_CK, iso3166_CR,
    iso3166_CI, iso3166_HR, iso3166_CU, iso3166_CW, iso3166_CY, iso3166_CZ,
    iso3166_DK, iso3166_DJ, iso3166_DM, iso3166_DO, iso3166_EC, iso3166_EG,
    iso3166_SV, iso3166_GQ, iso3166_ER, iso3166_EE, iso3166_SZ, iso3166_ET,
    iso3166_FK, iso3166_FO, iso3166_FJ, iso3166_FI, iso3166_FR, iso3166_GF,
    iso3166_PF, iso3166_TF, iso3166_GA, iso3166_GM, iso3166_GE, iso3166_DE,
    iso3166_GH, iso3166_GI, iso3166_GR, iso3166_GL, iso3166_GD, iso3166_GP,
    iso3166_GU, iso3166_GT, iso3166_GG, iso3166_GN, iso3166_GW, iso3166_GY,
    iso3166_HT, iso3166_HM, iso3166_VA, iso3166_HN, iso3166_HK, iso3166_HU,
    iso3166_IS, iso3166_IN, iso3166_ID, iso3166_IR, iso3166_IQ, iso3166_IE,
    iso3166_IM, iso3166_IL, iso3166_IT, iso3166_JM, iso3166_JP, iso3166_JE,
    iso3166_JO, iso3166_KZ, iso3166_KE, iso3166_KI, iso3166_KP, iso3166_KR,
    iso3166_KW, iso3166_KG, iso3166_LA, iso3166_LV, iso3166_LB, iso3166_LS,
    iso3166_LR, iso3166_LY, iso3166_LI, iso3166_LT, iso3166_LU, iso3166_MO,
    iso3166_MK, iso3166_MG, iso3166_MW, iso3166_MY, iso3166_MV, iso3166_ML,
    iso3166_MT, iso3166_MH, iso3166_MQ, iso3166_MR, iso3166_MU, iso3166_YT,
    iso3166_MX, iso3166_FM, iso3166_MD, iso3166_MC, iso3166_MN, iso3166_ME,
    iso3166_MS, iso3166_MA, iso3166_MZ, iso3166_MM, iso3166_NA, iso3166_NR,
    iso3166_NP, iso3166_NL, iso3166_NC, iso3166_NZ, iso3166_NI, iso3166_NE,
    iso3166_NG, iso3166_NU, iso3166_NF, iso3166_MP, iso3166_NO, iso3166_OM,
    iso3166_PK, iso3166_PW, iso3166_PS, iso3166_PA, iso3166_PG, iso3166_PY,
    iso3166_PE, iso3166_PH, iso3166_PN, iso3166_PL, iso3166_PT, iso3166_PR,
    iso3166_QA, iso3166_RE, iso3166_RO, iso3166_RU, iso3166_RW, iso3166_BL,
    iso3166_SH, iso3166_KN, iso3166_LC, iso3166_MF, iso3166_PM, iso3166_VC,
    iso3166_WS, iso3166_SM, iso3166_ST, iso3166_SA, iso3166_SN, iso3166_RS,
    iso3166_SC, iso3166_SL, iso3166_SG, iso3166_SX, iso3166_SK, iso3166_SI,
    iso3166_SB, iso3166_SO, iso3166_ZA, iso3166_GS, iso3166_SS, iso3166_ES,
    iso3166_LK, iso3166_SD, iso3166_SR, iso3166_SJ, iso3166_SE, iso3166_CH,
    iso3166_SY, iso3166_TW, iso3166_TJ, iso3166_TZ, iso3166_TH, iso3166_TL,
    iso3166_TG, iso3166_TK, iso3166_TO, iso3166_TT, iso3166_TN, iso3166_TR,
    iso3166_TM, iso3166_TC, iso3166_TV, iso3166_UG, iso3166_UA, iso3166_AE,
    iso3166_GB, iso3166_UM, iso3166_US, iso3166_UY, iso3166_UZ, iso3166_VU,
    iso3166_VE, iso3166_VN, iso3166_VG, iso3166_VI, iso3166_WF, iso3166_EH,
    iso3166_YE, iso3166_ZM, iso3166_ZW );

  TISO3166_Alpha3code = (iso3166_A3C_none,
    iso3166_AFG, iso3166_ALA, iso3166_ALB, iso3166_DZA, iso3166_ASM, iso3166_AND,
    iso3166_AGO, iso3166_AIA, iso3166_ATA, iso3166_ATG, iso3166_ARG, iso3166_ARM,
    iso3166_ABW, iso3166_AUS, iso3166_AUT, iso3166_AZE, iso3166_BHS, iso3166_BHR,
    iso3166_BGD, iso3166_BRB, iso3166_BLR, iso3166_BEL, iso3166_BLZ, iso3166_BEN,
    iso3166_BMU, iso3166_BTN, iso3166_BOL, iso3166_BES, iso3166_BIH, iso3166_BWA,
    iso3166_BVT, iso3166_BRA, iso3166_IOT, iso3166_BRN, iso3166_BGR, iso3166_BFA,
    iso3166_BDI, iso3166_CPV, iso3166_KHM, iso3166_CMR, iso3166_CAN, iso3166_CYM,
    iso3166_CAF, iso3166_TCD, iso3166_CHL, iso3166_CHN, iso3166_CXR, iso3166_CCK,
    iso3166_COL, iso3166_COM, iso3166_COD, iso3166_COG, iso3166_COK, iso3166_CRI,
    iso3166_CIV, iso3166_HRV, iso3166_CUB, iso3166_CUW, iso3166_CYP, iso3166_CZE,
    iso3166_DNK, iso3166_DJI, iso3166_DMA, iso3166_DOM, iso3166_ECU, iso3166_EGY,
    iso3166_SLV, iso3166_GNQ, iso3166_ERI, iso3166_EST, iso3166_SWZ, iso3166_ETH,
    iso3166_FLK, iso3166_FRO, iso3166_FJI, iso3166_FIN, iso3166_FRA, iso3166_GUF,
    iso3166_PYF, iso3166_ATF, iso3166_GAB, iso3166_GMB, iso3166_GEO, iso3166_DEU,
    iso3166_GHA, iso3166_GIB, iso3166_GRC, iso3166_GRL, iso3166_GRD, iso3166_GLP,
    iso3166_GUM, iso3166_GTM, iso3166_GGY, iso3166_GIN, iso3166_GNB, iso3166_GUY,
    iso3166_HTI, iso3166_HMD, iso3166_VAT, iso3166_HND, iso3166_HKG, iso3166_HUN,
    iso3166_ISL, iso3166_IND, iso3166_IDN, iso3166_IRN, iso3166_IRQ, iso3166_IRL,
    iso3166_IMN, iso3166_ISR, iso3166_ITA, iso3166_JAM, iso3166_JPN, iso3166_JEY,
    iso3166_JOR, iso3166_KAZ, iso3166_KEN, iso3166_KIR, iso3166_PRK, iso3166_KOR,
    iso3166_KWT, iso3166_KGZ, iso3166_LAO, iso3166_LVA, iso3166_LBN, iso3166_LSO,
    iso3166_LBR, iso3166_LBY, iso3166_LIE, iso3166_LTU, iso3166_LUX, iso3166_MAC,
    iso3166_MKD, iso3166_MDG, iso3166_MWI, iso3166_MYS, iso3166_MDV, iso3166_MLI,
    iso3166_MLT, iso3166_MHL, iso3166_MTQ, iso3166_MRT, iso3166_MUS, iso3166_MYT,
    iso3166_MEX, iso3166_FSM, iso3166_MDA, iso3166_MCO, iso3166_MNG, iso3166_MNE,
    iso3166_MSR, iso3166_MAR, iso3166_MOZ, iso3166_MMR, iso3166_NAM, iso3166_NRU,
    iso3166_NPL, iso3166_NLD, iso3166_NCL, iso3166_NZL, iso3166_NIC, iso3166_NER,
    iso3166_NGA, iso3166_NIU, iso3166_NFK, iso3166_MNP, iso3166_NOR, iso3166_OMN,
    iso3166_PAK, iso3166_PLW, iso3166_PSE, iso3166_PAN, iso3166_PNG, iso3166_PRY,
    iso3166_PER, iso3166_PHL, iso3166_PCN, iso3166_POL, iso3166_PRT, iso3166_PRI,
    iso3166_QAT, iso3166_REU, iso3166_ROU, iso3166_RUS, iso3166_RWA, iso3166_BLM,
    iso3166_SHN, iso3166_KNA, iso3166_LCA, iso3166_MAF, iso3166_SPM, iso3166_VCT,
    iso3166_WSM, iso3166_SMR, iso3166_STP, iso3166_SAU, iso3166_SEN, iso3166_SRB,
    iso3166_SYC, iso3166_SLE, iso3166_SGP, iso3166_SXM, iso3166_SVK, iso3166_SVN,
    iso3166_SLB, iso3166_SOM, iso3166_ZAF, iso3166_SGS, iso3166_SSD, iso3166_ESP,
    iso3166_LKA, iso3166_SDN, iso3166_SUR, iso3166_SJM, iso3166_SWE, iso3166_CHE,
    iso3166_SYR, iso3166_TWN, iso3166_TJK, iso3166_TZA, iso3166_THA, iso3166_TLS,
    iso3166_TGO, iso3166_TKL, iso3166_TON, iso3166_TTO, iso3166_TUN, iso3166_TUR,
    iso3166_TKM, iso3166_TCA, iso3166_TUV, iso3166_UGA, iso3166_UKR, iso3166_ARE,
    iso3166_GBR, iso3166_UMI, iso3166_USA, iso3166_URY, iso3166_UZB, iso3166_VUT,
    iso3166_VEN, iso3166_VNM, iso3166_VGB, iso3166_VIR, iso3166_WLF, iso3166_ESH,
    iso3166_YEM, iso3166_ZMB, iso3166_ZWE );

  TISO3166 = record
    Code: Word;
    ShortName: String[64];
    Alpha2code: TISO3166_Alpha2code;
    Alpha2codeStr: String[2];
    Alpha3code: TISO3166_Alpha3code;
    Alpha3codeStr: String[3];
  end;

const
  ISO3166List: array[0..249] of TISO3166 = (
    (Code: 0; ShortName: 'none'; Alpha2code: iso3166_A2C_none; Alpha2codeStr: ''; Alpha3code: iso3166_A3C_none; Alpha3codeStr: ''),
    (Code: 4; ShortName: 'Afghanistan'; Alpha2code: iso3166_AF; Alpha2codeStr: 'AF'; Alpha3code: iso3166_AFG; Alpha3codeStr: 'AFG'),
    (Code: 248; ShortName: 'Aland Islands'; Alpha2code: iso3166_AX; Alpha2codeStr: 'AX'; Alpha3code: iso3166_ALA; Alpha3codeStr: 'ALA'),
    (Code: 8; ShortName: 'Albania'; Alpha2code: iso3166_AL; Alpha2codeStr: 'AL'; Alpha3code: iso3166_ALB; Alpha3codeStr: 'ALB'),
    (Code: 12; ShortName: 'Algeria'; Alpha2code: iso3166_DZ; Alpha2codeStr: 'DZ'; Alpha3code: iso3166_DZA; Alpha3codeStr: 'DZA'),
    (Code: 16; ShortName: 'American Samoa'; Alpha2code: iso3166_AS; Alpha2codeStr: 'AS'; Alpha3code: iso3166_ASM; Alpha3codeStr: 'ASM'),
    (Code: 20; ShortName: 'Andorra'; Alpha2code: iso3166_AD; Alpha2codeStr: 'AD'; Alpha3code: iso3166_AND; Alpha3codeStr: 'AND'),
    (Code: 24; ShortName: 'Angola'; Alpha2code: iso3166_AO; Alpha2codeStr: 'AO'; Alpha3code: iso3166_AGO; Alpha3codeStr: 'AGO'),
    (Code: 660; ShortName: 'Anguilla'; Alpha2code: iso3166_AI; Alpha2codeStr: 'AI'; Alpha3code: iso3166_AIA; Alpha3codeStr: 'AIA'),
    (Code: 10; ShortName: 'Antarctica'; Alpha2code: iso3166_AQ; Alpha2codeStr: 'AQ'; Alpha3code: iso3166_ATA; Alpha3codeStr: 'ATA'),
    (Code: 28; ShortName: 'Antigua and Barbuda'; Alpha2code: iso3166_AG; Alpha2codeStr: 'AG'; Alpha3code: iso3166_ATG; Alpha3codeStr: 'ATG'),
    (Code: 32; ShortName: 'Argentina'; Alpha2code: iso3166_AR; Alpha2codeStr: 'AR'; Alpha3code: iso3166_ARG; Alpha3codeStr: 'ARG'),
    (Code: 51; ShortName: 'Armenia'; Alpha2code: iso3166_AM; Alpha2codeStr: 'AM'; Alpha3code: iso3166_ARM; Alpha3codeStr: 'ARM'),
    (Code: 533; ShortName: 'Aruba'; Alpha2code: iso3166_AW; Alpha2codeStr: 'AW'; Alpha3code: iso3166_ABW; Alpha3codeStr: 'ABW'),
    (Code: 36; ShortName: 'Australia'; Alpha2code: iso3166_AU; Alpha2codeStr: 'AU'; Alpha3code: iso3166_AUS; Alpha3codeStr: 'AUS'),
    (Code: 40; ShortName: 'Austria'; Alpha2code: iso3166_AT; Alpha2codeStr: 'AT'; Alpha3code: iso3166_AUT; Alpha3codeStr: 'AUT'),
    (Code: 31; ShortName: 'Azerbaijan'; Alpha2code: iso3166_AZ; Alpha2codeStr: 'AZ'; Alpha3code: iso3166_AZE; Alpha3codeStr: 'AZE'),
    (Code: 44; ShortName: 'Bahamas (the)'; Alpha2code: iso3166_BS; Alpha2codeStr: 'BS'; Alpha3code: iso3166_BHS; Alpha3codeStr: 'BHS'),
    (Code: 48; ShortName: 'Bahrain'; Alpha2code: iso3166_BH; Alpha2codeStr: 'BH'; Alpha3code: iso3166_BHR; Alpha3codeStr: 'BHR'),
    (Code: 50; ShortName: 'Bangladesh'; Alpha2code: iso3166_BD; Alpha2codeStr: 'BD'; Alpha3code: iso3166_BGD; Alpha3codeStr: 'BGD'),
    (Code: 52; ShortName: 'Barbados'; Alpha2code: iso3166_BB; Alpha2codeStr: 'BB'; Alpha3code: iso3166_BRB; Alpha3codeStr: 'BRB'),
    (Code: 112; ShortName: 'Belarus'; Alpha2code: iso3166_BY; Alpha2codeStr: 'BY'; Alpha3code: iso3166_BLR; Alpha3codeStr: 'BLR'),
    (Code: 56; ShortName: 'Belgium'; Alpha2code: iso3166_BE; Alpha2codeStr: 'BE'; Alpha3code: iso3166_BEL; Alpha3codeStr: 'BEL'),
    (Code: 84; ShortName: 'Belize'; Alpha2code: iso3166_BZ; Alpha2codeStr: 'BZ'; Alpha3code: iso3166_BLZ; Alpha3codeStr: 'BLZ'),
    (Code: 204; ShortName: 'Benin'; Alpha2code: iso3166_BJ; Alpha2codeStr: 'BJ'; Alpha3code: iso3166_BEN; Alpha3codeStr: 'BEN'),
    (Code: 60; ShortName: 'Bermuda'; Alpha2code: iso3166_BM; Alpha2codeStr: 'BM'; Alpha3code: iso3166_BMU; Alpha3codeStr: 'BMU'),
    (Code: 64; ShortName: 'Bhutan'; Alpha2code: iso3166_BT; Alpha2codeStr: 'BT'; Alpha3code: iso3166_BTN; Alpha3codeStr: 'BTN'),
    (Code: 68; ShortName: 'Bolivia (Plurinational State of)'; Alpha2code: iso3166_BO; Alpha2codeStr: 'BO'; Alpha3code: iso3166_BOL; Alpha3codeStr: 'BOL'),
    (Code: 535; ShortName: 'Bonaire, Sint Eustatius and Saba'; Alpha2code: iso3166_BQ; Alpha2codeStr: 'BQ'; Alpha3code: iso3166_BES; Alpha3codeStr: 'BES'),
    (Code: 70; ShortName: 'Bosnia and Herzegovina'; Alpha2code: iso3166_BA; Alpha2codeStr: 'BA'; Alpha3code: iso3166_BIH; Alpha3codeStr: 'BIH'),
    (Code: 72; ShortName: 'Botswana'; Alpha2code: iso3166_BW; Alpha2codeStr: 'BW'; Alpha3code: iso3166_BWA; Alpha3codeStr: 'BWA'),
    (Code: 74; ShortName: 'Bouvet Island'; Alpha2code: iso3166_BV; Alpha2codeStr: 'BV'; Alpha3code: iso3166_BVT; Alpha3codeStr: 'BVT'),
    (Code: 76; ShortName: 'Brazil'; Alpha2code: iso3166_BR; Alpha2codeStr: 'BR'; Alpha3code: iso3166_BRA; Alpha3codeStr: 'BRA'),
    (Code: 86; ShortName: 'British Indian Ocean Territory (the)'; Alpha2code: iso3166_IO; Alpha2codeStr: 'IO'; Alpha3code: iso3166_IOT; Alpha3codeStr: 'IOT'),
    (Code: 96; ShortName: 'Brunei Darussalam'; Alpha2code: iso3166_BN; Alpha2codeStr: 'BN'; Alpha3code: iso3166_BRN; Alpha3codeStr: 'BRN'),
    (Code: 100; ShortName: 'Bulgaria'; Alpha2code: iso3166_BG; Alpha2codeStr: 'BG'; Alpha3code: iso3166_BGR; Alpha3codeStr: 'BGR'),
    (Code: 854; ShortName: 'Burkina Faso'; Alpha2code: iso3166_BF; Alpha2codeStr: 'BF'; Alpha3code: iso3166_BFA; Alpha3codeStr: 'BFA'),
    (Code: 108; ShortName: 'Burundi'; Alpha2code: iso3166_BI; Alpha2codeStr: 'BI'; Alpha3code: iso3166_BDI; Alpha3codeStr: 'BDI'),
    (Code: 132; ShortName: 'Cabo Verde'; Alpha2code: iso3166_CV; Alpha2codeStr: 'CV'; Alpha3code: iso3166_CPV; Alpha3codeStr: 'CPV'),
    (Code: 116; ShortName: 'Cambodia'; Alpha2code: iso3166_KH; Alpha2codeStr: 'KH'; Alpha3code: iso3166_KHM; Alpha3codeStr: 'KHM'),
    (Code: 120; ShortName: 'Cameroon'; Alpha2code: iso3166_CM; Alpha2codeStr: 'CM'; Alpha3code: iso3166_CMR; Alpha3codeStr: 'CMR'),
    (Code: 124; ShortName: 'Canada'; Alpha2code: iso3166_CA; Alpha2codeStr: 'CA'; Alpha3code: iso3166_CAN; Alpha3codeStr: 'CAN'),
    (Code: 136; ShortName: 'Cayman Islands (the)'; Alpha2code: iso3166_KY; Alpha2codeStr: 'KY'; Alpha3code: iso3166_CYM; Alpha3codeStr: 'CYM'),
    (Code: 140; ShortName: 'Central African Republic (the)'; Alpha2code: iso3166_CF; Alpha2codeStr: 'CF'; Alpha3code: iso3166_CAF; Alpha3codeStr: 'CAF'),
    (Code: 148; ShortName: 'Chad'; Alpha2code: iso3166_TD; Alpha2codeStr: 'TD'; Alpha3code: iso3166_TCD; Alpha3codeStr: 'TCD'),
    (Code: 152; ShortName: 'Chile'; Alpha2code: iso3166_CL; Alpha2codeStr: 'CL'; Alpha3code: iso3166_CHL; Alpha3codeStr: 'CHL'),
    (Code: 156; ShortName: 'China'; Alpha2code: iso3166_CN; Alpha2codeStr: 'CN'; Alpha3code: iso3166_CHN; Alpha3codeStr: 'CHN'),
    (Code: 162; ShortName: 'Christmas Island'; Alpha2code: iso3166_CX; Alpha2codeStr: 'CX'; Alpha3code: iso3166_CXR; Alpha3codeStr: 'CXR'),
    (Code: 166; ShortName: 'Cocos (Keeling) Islands (the)'; Alpha2code: iso3166_CC; Alpha2codeStr: 'CC'; Alpha3code: iso3166_CCK; Alpha3codeStr: 'CCK'),
    (Code: 170; ShortName: 'Colombia'; Alpha2code: iso3166_CO; Alpha2codeStr: 'CO'; Alpha3code: iso3166_COL; Alpha3codeStr: 'COL'),
    (Code: 174; ShortName: 'Comoros (the)'; Alpha2code: iso3166_KM; Alpha2codeStr: 'KM'; Alpha3code: iso3166_COM; Alpha3codeStr: 'COM'),
    (Code: 180; ShortName: 'Congo (the Democratic Republic of the)'; Alpha2code: iso3166_CD; Alpha2codeStr: 'CD'; Alpha3code: iso3166_COD; Alpha3codeStr: 'COD'),
    (Code: 178; ShortName: 'Congo (the)'; Alpha2code: iso3166_CG; Alpha2codeStr: 'CG'; Alpha3code: iso3166_COG; Alpha3codeStr: 'COG'),
    (Code: 184; ShortName: 'Cook Islands (the)'; Alpha2code: iso3166_CK; Alpha2codeStr: 'CK'; Alpha3code: iso3166_COK; Alpha3codeStr: 'COK'),
    (Code: 188; ShortName: 'Costa Rica'; Alpha2code: iso3166_CR; Alpha2codeStr: 'CR'; Alpha3code: iso3166_CRI; Alpha3codeStr: 'CRI'),
    (Code: 384; ShortName: 'Cote d''Ivoire'; Alpha2code: iso3166_CI; Alpha2codeStr: 'CI'; Alpha3code: iso3166_CIV; Alpha3codeStr: 'CIV'),
    (Code: 191; ShortName: 'Croatia'; Alpha2code: iso3166_HR; Alpha2codeStr: 'HR'; Alpha3code: iso3166_HRV; Alpha3codeStr: 'HRV'),
    (Code: 192; ShortName: 'Cuba'; Alpha2code: iso3166_CU; Alpha2codeStr: 'CU'; Alpha3code: iso3166_CUB; Alpha3codeStr: 'CUB'),
    (Code: 531; ShortName: 'Curacao'; Alpha2code: iso3166_CW; Alpha2codeStr: 'CW'; Alpha3code: iso3166_CUW; Alpha3codeStr: 'CUW'),
    (Code: 196; ShortName: 'Cyprus'; Alpha2code: iso3166_CY; Alpha2codeStr: 'CY'; Alpha3code: iso3166_CYP; Alpha3codeStr: 'CYP'),
    (Code: 203; ShortName: 'Czechia'; Alpha2code: iso3166_CZ; Alpha2codeStr: 'CZ'; Alpha3code: iso3166_CZE; Alpha3codeStr: 'CZE'),
    (Code: 208; ShortName: 'Denmark'; Alpha2code: iso3166_DK; Alpha2codeStr: 'DK'; Alpha3code: iso3166_DNK; Alpha3codeStr: 'DNK'),
    (Code: 262; ShortName: 'Djibouti'; Alpha2code: iso3166_DJ; Alpha2codeStr: 'DJ'; Alpha3code: iso3166_DJI; Alpha3codeStr: 'DJI'),
    (Code: 212; ShortName: 'Dominica'; Alpha2code: iso3166_DM; Alpha2codeStr: 'DM'; Alpha3code: iso3166_DMA; Alpha3codeStr: 'DMA'),
    (Code: 214; ShortName: 'Dominican Republic (the)'; Alpha2code: iso3166_DO; Alpha2codeStr: 'DO'; Alpha3code: iso3166_DOM; Alpha3codeStr: 'DOM'),
    (Code: 218; ShortName: 'Ecuador'; Alpha2code: iso3166_EC; Alpha2codeStr: 'EC'; Alpha3code: iso3166_ECU; Alpha3codeStr: 'ECU'),
    (Code: 818; ShortName: 'Egypt'; Alpha2code: iso3166_EG; Alpha2codeStr: 'EG'; Alpha3code: iso3166_EGY; Alpha3codeStr: 'EGY'),
    (Code: 222; ShortName: 'El Salvador'; Alpha2code: iso3166_SV; Alpha2codeStr: 'SV'; Alpha3code: iso3166_SLV; Alpha3codeStr: 'SLV'),
    (Code: 226; ShortName: 'Equatorial Guinea'; Alpha2code: iso3166_GQ; Alpha2codeStr: 'GQ'; Alpha3code: iso3166_GNQ; Alpha3codeStr: 'GNQ'),
    (Code: 232; ShortName: 'Eritrea'; Alpha2code: iso3166_ER; Alpha2codeStr: 'ER'; Alpha3code: iso3166_ERI; Alpha3codeStr: 'ERI'),
    (Code: 233; ShortName: 'Estonia'; Alpha2code: iso3166_EE; Alpha2codeStr: 'EE'; Alpha3code: iso3166_EST; Alpha3codeStr: 'EST'),
    (Code: 748; ShortName: 'Eswatini'; Alpha2code: iso3166_SZ; Alpha2codeStr: 'SZ'; Alpha3code: iso3166_SWZ; Alpha3codeStr: 'SWZ'),
    (Code: 231; ShortName: 'Ethiopia'; Alpha2code: iso3166_ET; Alpha2codeStr: 'ET'; Alpha3code: iso3166_ETH; Alpha3codeStr: 'ETH'),
    (Code: 238; ShortName: 'Falkland Islands (the) [Malvinas]'; Alpha2code: iso3166_FK; Alpha2codeStr: 'FK'; Alpha3code: iso3166_FLK; Alpha3codeStr: 'FLK'),
    (Code: 234; ShortName: 'Faroe Islands (the)'; Alpha2code: iso3166_FO; Alpha2codeStr: 'FO'; Alpha3code: iso3166_FRO; Alpha3codeStr: 'FRO'),
    (Code: 242; ShortName: 'Fiji'; Alpha2code: iso3166_FJ; Alpha2codeStr: 'FJ'; Alpha3code: iso3166_FJI; Alpha3codeStr: 'FJI'),
    (Code: 246; ShortName: 'Finland'; Alpha2code: iso3166_FI; Alpha2codeStr: 'FI'; Alpha3code: iso3166_FIN; Alpha3codeStr: 'FIN'),
    (Code: 250; ShortName: 'France'; Alpha2code: iso3166_FR; Alpha2codeStr: 'FR'; Alpha3code: iso3166_FRA; Alpha3codeStr: 'FRA'),
    (Code: 254; ShortName: 'French Guiana'; Alpha2code: iso3166_GF; Alpha2codeStr: 'GF'; Alpha3code: iso3166_GUF; Alpha3codeStr: 'GUF'),
    (Code: 258; ShortName: 'French Polynesia'; Alpha2code: iso3166_PF; Alpha2codeStr: 'PF'; Alpha3code: iso3166_PYF; Alpha3codeStr: 'PYF'),
    (Code: 260; ShortName: 'French Southern Territories (the)'; Alpha2code: iso3166_TF; Alpha2codeStr: 'TF'; Alpha3code: iso3166_ATF; Alpha3codeStr: 'ATF'),
    (Code: 266; ShortName: 'Gabon'; Alpha2code: iso3166_GA; Alpha2codeStr: 'GA'; Alpha3code: iso3166_GAB; Alpha3codeStr: 'GAB'),
    (Code: 270; ShortName: 'Gambia (the)'; Alpha2code: iso3166_GM; Alpha2codeStr: 'GM'; Alpha3code: iso3166_GMB; Alpha3codeStr: 'GMB'),
    (Code: 268; ShortName: 'Georgia'; Alpha2code: iso3166_GE; Alpha2codeStr: 'GE'; Alpha3code: iso3166_GEO; Alpha3codeStr: 'GEO'),
    (Code: 276; ShortName: 'Germany'; Alpha2code: iso3166_DE; Alpha2codeStr: 'DE'; Alpha3code: iso3166_DEU; Alpha3codeStr: 'DEU'),
    (Code: 288; ShortName: 'Ghana'; Alpha2code: iso3166_GH; Alpha2codeStr: 'GH'; Alpha3code: iso3166_GHA; Alpha3codeStr: 'GHA'),
    (Code: 292; ShortName: 'Gibraltar'; Alpha2code: iso3166_GI; Alpha2codeStr: 'GI'; Alpha3code: iso3166_GIB; Alpha3codeStr: 'GIB'),
    (Code: 300; ShortName: 'Greece'; Alpha2code: iso3166_GR; Alpha2codeStr: 'GR'; Alpha3code: iso3166_GRC; Alpha3codeStr: 'GRC'),
    (Code: 304; ShortName: 'Greenland'; Alpha2code: iso3166_GL; Alpha2codeStr: 'GL'; Alpha3code: iso3166_GRL; Alpha3codeStr: 'GRL'),
    (Code: 308; ShortName: 'Grenada'; Alpha2code: iso3166_GD; Alpha2codeStr: 'GD'; Alpha3code: iso3166_GRD; Alpha3codeStr: 'GRD'),
    (Code: 312; ShortName: 'Guadeloupe'; Alpha2code: iso3166_GP; Alpha2codeStr: 'GP'; Alpha3code: iso3166_GLP; Alpha3codeStr: 'GLP'),
    (Code: 316; ShortName: 'Guam'; Alpha2code: iso3166_GU; Alpha2codeStr: 'GU'; Alpha3code: iso3166_GUM; Alpha3codeStr: 'GUM'),
    (Code: 320; ShortName: 'Guatemala'; Alpha2code: iso3166_GT; Alpha2codeStr: 'GT'; Alpha3code: iso3166_GTM; Alpha3codeStr: 'GTM'),
    (Code: 831; ShortName: 'Guernsey'; Alpha2code: iso3166_GG; Alpha2codeStr: 'GG'; Alpha3code: iso3166_GGY; Alpha3codeStr: 'GGY'),
    (Code: 324; ShortName: 'Guinea'; Alpha2code: iso3166_GN; Alpha2codeStr: 'GN'; Alpha3code: iso3166_GIN; Alpha3codeStr: 'GIN'),
    (Code: 624; ShortName: 'Guinea-Bissau'; Alpha2code: iso3166_GW; Alpha2codeStr: 'GW'; Alpha3code: iso3166_GNB; Alpha3codeStr: 'GNB'),
    (Code: 328; ShortName: 'Guyana'; Alpha2code: iso3166_GY; Alpha2codeStr: 'GY'; Alpha3code: iso3166_GUY; Alpha3codeStr: 'GUY'),
    (Code: 332; ShortName: 'Haiti'; Alpha2code: iso3166_HT; Alpha2codeStr: 'HT'; Alpha3code: iso3166_HTI; Alpha3codeStr: 'HTI'),
    (Code: 334; ShortName: 'Heard Island and McDonald Islands'; Alpha2code: iso3166_HM; Alpha2codeStr: 'HM'; Alpha3code: iso3166_HMD; Alpha3codeStr: 'HMD'),
    (Code: 336; ShortName: 'Holy See (the)'; Alpha2code: iso3166_VA; Alpha2codeStr: 'VA'; Alpha3code: iso3166_VAT; Alpha3codeStr: 'VAT'),
    (Code: 340; ShortName: 'Honduras'; Alpha2code: iso3166_HN; Alpha2codeStr: 'HN'; Alpha3code: iso3166_HND; Alpha3codeStr: 'HND'),
    (Code: 344; ShortName: 'Hong Kong'; Alpha2code: iso3166_HK; Alpha2codeStr: 'HK'; Alpha3code: iso3166_HKG; Alpha3codeStr: 'HKG'),
    (Code: 348; ShortName: 'Hungary'; Alpha2code: iso3166_HU; Alpha2codeStr: 'HU'; Alpha3code: iso3166_HUN; Alpha3codeStr: 'HUN'),
    (Code: 352; ShortName: 'Iceland'; Alpha2code: iso3166_IS; Alpha2codeStr: 'IS'; Alpha3code: iso3166_ISL; Alpha3codeStr: 'ISL'),
    (Code: 356; ShortName: 'India'; Alpha2code: iso3166_IN; Alpha2codeStr: 'IN'; Alpha3code: iso3166_IND; Alpha3codeStr: 'IND'),
    (Code: 360; ShortName: 'Indonesia'; Alpha2code: iso3166_ID; Alpha2codeStr: 'ID'; Alpha3code: iso3166_IDN; Alpha3codeStr: 'IDN'),
    (Code: 364; ShortName: 'Iran (Islamic Republic of)'; Alpha2code: iso3166_IR; Alpha2codeStr: 'IR'; Alpha3code: iso3166_IRN; Alpha3codeStr: 'IRN'),
    (Code: 368; ShortName: 'Iraq'; Alpha2code: iso3166_IQ; Alpha2codeStr: 'IQ'; Alpha3code: iso3166_IRQ; Alpha3codeStr: 'IRQ'),
    (Code: 372; ShortName: 'Ireland'; Alpha2code: iso3166_IE; Alpha2codeStr: 'IE'; Alpha3code: iso3166_IRL; Alpha3codeStr: 'IRL'),
    (Code: 833; ShortName: 'Isle of Man'; Alpha2code: iso3166_IM; Alpha2codeStr: 'IM'; Alpha3code: iso3166_IMN; Alpha3codeStr: 'IMN'),
    (Code: 376; ShortName: 'Israel'; Alpha2code: iso3166_IL; Alpha2codeStr: 'IL'; Alpha3code: iso3166_ISR; Alpha3codeStr: 'ISR'),
    (Code: 380; ShortName: 'Italy'; Alpha2code: iso3166_IT; Alpha2codeStr: 'IT'; Alpha3code: iso3166_ITA; Alpha3codeStr: 'ITA'),
    (Code: 388; ShortName: 'Jamaica'; Alpha2code: iso3166_JM; Alpha2codeStr: 'JM'; Alpha3code: iso3166_JAM; Alpha3codeStr: 'JAM'),
    (Code: 392; ShortName: 'Japan'; Alpha2code: iso3166_JP; Alpha2codeStr: 'JP'; Alpha3code: iso3166_JPN; Alpha3codeStr: 'JPN'),
    (Code: 832; ShortName: 'Jersey'; Alpha2code: iso3166_JE; Alpha2codeStr: 'JE'; Alpha3code: iso3166_JEY; Alpha3codeStr: 'JEY'),
    (Code: 400; ShortName: 'Jordan'; Alpha2code: iso3166_JO; Alpha2codeStr: 'JO'; Alpha3code: iso3166_JOR; Alpha3codeStr: 'JOR'),
    (Code: 398; ShortName: 'Kazakhstan'; Alpha2code: iso3166_KZ; Alpha2codeStr: 'KZ'; Alpha3code: iso3166_KAZ; Alpha3codeStr: 'KAZ'),
    (Code: 404; ShortName: 'Kenya'; Alpha2code: iso3166_KE; Alpha2codeStr: 'KE'; Alpha3code: iso3166_KEN; Alpha3codeStr: 'KEN'),
    (Code: 296; ShortName: 'Kiribati'; Alpha2code: iso3166_KI; Alpha2codeStr: 'KI'; Alpha3code: iso3166_KIR; Alpha3codeStr: 'KIR'),
    (Code: 408; ShortName: 'Korea (the Democratic People''s Republic of)'; Alpha2code: iso3166_KP; Alpha2codeStr: 'KP'; Alpha3code: iso3166_PRK; Alpha3codeStr: 'PRK'),
    (Code: 410; ShortName: 'Korea (the Republic of)'; Alpha2code: iso3166_KR; Alpha2codeStr: 'KR'; Alpha3code: iso3166_KOR; Alpha3codeStr: 'KOR'),
    (Code: 414; ShortName: 'Kuwait'; Alpha2code: iso3166_KW; Alpha2codeStr: 'KW'; Alpha3code: iso3166_KWT; Alpha3codeStr: 'KWT'),
    (Code: 417; ShortName: 'Kyrgyzstan'; Alpha2code: iso3166_KG; Alpha2codeStr: 'KG'; Alpha3code: iso3166_KGZ; Alpha3codeStr: 'KGZ'),
    (Code: 418; ShortName: 'Lao People''s Democratic Republic (the)'; Alpha2code: iso3166_LA; Alpha2codeStr: 'LA'; Alpha3code: iso3166_LAO; Alpha3codeStr: 'LAO'),
    (Code: 428; ShortName: 'Latvia'; Alpha2code: iso3166_LV; Alpha2codeStr: 'LV'; Alpha3code: iso3166_LVA; Alpha3codeStr: 'LVA'),
    (Code: 422; ShortName: 'Lebanon'; Alpha2code: iso3166_LB; Alpha2codeStr: 'LB'; Alpha3code: iso3166_LBN; Alpha3codeStr: 'LBN'),
    (Code: 426; ShortName: 'Lesotho'; Alpha2code: iso3166_LS; Alpha2codeStr: 'LS'; Alpha3code: iso3166_LSO; Alpha3codeStr: 'LSO'),
    (Code: 430; ShortName: 'Liberia'; Alpha2code: iso3166_LR; Alpha2codeStr: 'LR'; Alpha3code: iso3166_LBR; Alpha3codeStr: 'LBR'),
    (Code: 434; ShortName: 'Libya'; Alpha2code: iso3166_LY; Alpha2codeStr: 'LY'; Alpha3code: iso3166_LBY; Alpha3codeStr: 'LBY'),
    (Code: 438; ShortName: 'Liechtenstein'; Alpha2code: iso3166_LI; Alpha2codeStr: 'LI'; Alpha3code: iso3166_LIE; Alpha3codeStr: 'LIE'),
    (Code: 440; ShortName: 'Lithuania'; Alpha2code: iso3166_LT; Alpha2codeStr: 'LT'; Alpha3code: iso3166_LTU; Alpha3codeStr: 'LTU'),
    (Code: 442; ShortName: 'Luxembourg'; Alpha2code: iso3166_LU; Alpha2codeStr: 'LU'; Alpha3code: iso3166_LUX; Alpha3codeStr: 'LUX'),
    (Code: 446; ShortName: 'Macao'; Alpha2code: iso3166_MO; Alpha2codeStr: 'MO'; Alpha3code: iso3166_MAC; Alpha3codeStr: 'MAC'),
    (Code: 807; ShortName: 'Macedonia (the former Yugoslav Republic of)'; Alpha2code: iso3166_MK; Alpha2codeStr: 'MK'; Alpha3code: iso3166_MKD; Alpha3codeStr: 'MKD'),
    (Code: 450; ShortName: 'Madagascar'; Alpha2code: iso3166_MG; Alpha2codeStr: 'MG'; Alpha3code: iso3166_MDG; Alpha3codeStr: 'MDG'),
    (Code: 454; ShortName: 'Malawi'; Alpha2code: iso3166_MW; Alpha2codeStr: 'MW'; Alpha3code: iso3166_MWI; Alpha3codeStr: 'MWI'),
    (Code: 458; ShortName: 'Malaysia'; Alpha2code: iso3166_MY; Alpha2codeStr: 'MY'; Alpha3code: iso3166_MYS; Alpha3codeStr: 'MYS'),
    (Code: 462; ShortName: 'Maldives'; Alpha2code: iso3166_MV; Alpha2codeStr: 'MV'; Alpha3code: iso3166_MDV; Alpha3codeStr: 'MDV'),
    (Code: 466; ShortName: 'Mali'; Alpha2code: iso3166_ML; Alpha2codeStr: 'ML'; Alpha3code: iso3166_MLI; Alpha3codeStr: 'MLI'),
    (Code: 470; ShortName: 'Malta'; Alpha2code: iso3166_MT; Alpha2codeStr: 'MT'; Alpha3code: iso3166_MLT; Alpha3codeStr: 'MLT'),
    (Code: 584; ShortName: 'Marshall Islands (the)'; Alpha2code: iso3166_MH; Alpha2codeStr: 'MH'; Alpha3code: iso3166_MHL; Alpha3codeStr: 'MHL'),
    (Code: 474; ShortName: 'Martinique'; Alpha2code: iso3166_MQ; Alpha2codeStr: 'MQ'; Alpha3code: iso3166_MTQ; Alpha3codeStr: 'MTQ'),
    (Code: 478; ShortName: 'Mauritania'; Alpha2code: iso3166_MR; Alpha2codeStr: 'MR'; Alpha3code: iso3166_MRT; Alpha3codeStr: 'MRT'),
    (Code: 480; ShortName: 'Mauritius'; Alpha2code: iso3166_MU; Alpha2codeStr: 'MU'; Alpha3code: iso3166_MUS; Alpha3codeStr: 'MUS'),
    (Code: 175; ShortName: 'Mayotte'; Alpha2code: iso3166_YT; Alpha2codeStr: 'YT'; Alpha3code: iso3166_MYT; Alpha3codeStr: 'MYT'),
    (Code: 484; ShortName: 'Mexico'; Alpha2code: iso3166_MX; Alpha2codeStr: 'MX'; Alpha3code: iso3166_MEX; Alpha3codeStr: 'MEX'),
    (Code: 583; ShortName: 'Micronesia (Federated States of)'; Alpha2code: iso3166_FM; Alpha2codeStr: 'FM'; Alpha3code: iso3166_FSM; Alpha3codeStr: 'FSM'),
    (Code: 498; ShortName: 'Moldova (the Republic of)'; Alpha2code: iso3166_MD; Alpha2codeStr: 'MD'; Alpha3code: iso3166_MDA; Alpha3codeStr: 'MDA'),
    (Code: 492; ShortName: 'Monaco'; Alpha2code: iso3166_MC; Alpha2codeStr: 'MC'; Alpha3code: iso3166_MCO; Alpha3codeStr: 'MCO'),
    (Code: 496; ShortName: 'Mongolia'; Alpha2code: iso3166_MN; Alpha2codeStr: 'MN'; Alpha3code: iso3166_MNG; Alpha3codeStr: 'MNG'),
    (Code: 499; ShortName: 'Montenegro'; Alpha2code: iso3166_ME; Alpha2codeStr: 'ME'; Alpha3code: iso3166_MNE; Alpha3codeStr: 'MNE'),
    (Code: 500; ShortName: 'Montserrat'; Alpha2code: iso3166_MS; Alpha2codeStr: 'MS'; Alpha3code: iso3166_MSR; Alpha3codeStr: 'MSR'),
    (Code: 504; ShortName: 'Morocco'; Alpha2code: iso3166_MA; Alpha2codeStr: 'MA'; Alpha3code: iso3166_MAR; Alpha3codeStr: 'MAR'),
    (Code: 508; ShortName: 'Mozambique'; Alpha2code: iso3166_MZ; Alpha2codeStr: 'MZ'; Alpha3code: iso3166_MOZ; Alpha3codeStr: 'MOZ'),
    (Code: 104; ShortName: 'Myanmar'; Alpha2code: iso3166_MM; Alpha2codeStr: 'MM'; Alpha3code: iso3166_MMR; Alpha3codeStr: 'MMR'),
    (Code: 516; ShortName: 'Namibia'; Alpha2code: iso3166_NA; Alpha2codeStr: 'NA'; Alpha3code: iso3166_NAM; Alpha3codeStr: 'NAM'),
    (Code: 520; ShortName: 'Nauru'; Alpha2code: iso3166_NR; Alpha2codeStr: 'NR'; Alpha3code: iso3166_NRU; Alpha3codeStr: 'NRU'),
    (Code: 524; ShortName: 'Nepal'; Alpha2code: iso3166_NP; Alpha2codeStr: 'NP'; Alpha3code: iso3166_NPL; Alpha3codeStr: 'NPL'),
    (Code: 528; ShortName: 'Netherlands (the)'; Alpha2code: iso3166_NL; Alpha2codeStr: 'NL'; Alpha3code: iso3166_NLD; Alpha3codeStr: 'NLD'),
    (Code: 540; ShortName: 'New Caledonia'; Alpha2code: iso3166_NC; Alpha2codeStr: 'NC'; Alpha3code: iso3166_NCL; Alpha3codeStr: 'NCL'),
    (Code: 554; ShortName: 'New Zealand'; Alpha2code: iso3166_NZ; Alpha2codeStr: 'NZ'; Alpha3code: iso3166_NZL; Alpha3codeStr: 'NZL'),
    (Code: 558; ShortName: 'Nicaragua'; Alpha2code: iso3166_NI; Alpha2codeStr: 'NI'; Alpha3code: iso3166_NIC; Alpha3codeStr: 'NIC'),
    (Code: 562; ShortName: 'Niger (the)'; Alpha2code: iso3166_NE; Alpha2codeStr: 'NE'; Alpha3code: iso3166_NER; Alpha3codeStr: 'NER'),
    (Code: 566; ShortName: 'Nigeria'; Alpha2code: iso3166_NG; Alpha2codeStr: 'NG'; Alpha3code: iso3166_NGA; Alpha3codeStr: 'NGA'),
    (Code: 570; ShortName: 'Niue'; Alpha2code: iso3166_NU; Alpha2codeStr: 'NU'; Alpha3code: iso3166_NIU; Alpha3codeStr: 'NIU'),
    (Code: 574; ShortName: 'Norfolk Island'; Alpha2code: iso3166_NF; Alpha2codeStr: 'NF'; Alpha3code: iso3166_NFK; Alpha3codeStr: 'NFK'),
    (Code: 580; ShortName: 'Northern Mariana Islands (the)'; Alpha2code: iso3166_MP; Alpha2codeStr: 'MP'; Alpha3code: iso3166_MNP; Alpha3codeStr: 'MNP'),
    (Code: 578; ShortName: 'Norway'; Alpha2code: iso3166_NO; Alpha2codeStr: 'NO'; Alpha3code: iso3166_NOR; Alpha3codeStr: 'NOR'),
    (Code: 512; ShortName: 'Oman'; Alpha2code: iso3166_OM; Alpha2codeStr: 'OM'; Alpha3code: iso3166_OMN; Alpha3codeStr: 'OMN'),
    (Code: 586; ShortName: 'Pakistan'; Alpha2code: iso3166_PK; Alpha2codeStr: 'PK'; Alpha3code: iso3166_PAK; Alpha3codeStr: 'PAK'),
    (Code: 585; ShortName: 'Palau'; Alpha2code: iso3166_PW; Alpha2codeStr: 'PW'; Alpha3code: iso3166_PLW; Alpha3codeStr: 'PLW'),
    (Code: 275; ShortName: 'Palestine, State of'; Alpha2code: iso3166_PS; Alpha2codeStr: 'PS'; Alpha3code: iso3166_PSE; Alpha3codeStr: 'PSE'),
    (Code: 591; ShortName: 'Panama'; Alpha2code: iso3166_PA; Alpha2codeStr: 'PA'; Alpha3code: iso3166_PAN; Alpha3codeStr: 'PAN'),
    (Code: 598; ShortName: 'Papua New Guinea'; Alpha2code: iso3166_PG; Alpha2codeStr: 'PG'; Alpha3code: iso3166_PNG; Alpha3codeStr: 'PNG'),
    (Code: 600; ShortName: 'Paraguay'; Alpha2code: iso3166_PY; Alpha2codeStr: 'PY'; Alpha3code: iso3166_PRY; Alpha3codeStr: 'PRY'),
    (Code: 604; ShortName: 'Peru'; Alpha2code: iso3166_PE; Alpha2codeStr: 'PE'; Alpha3code: iso3166_PER; Alpha3codeStr: 'PER'),
    (Code: 608; ShortName: 'Philippines (the)'; Alpha2code: iso3166_PH; Alpha2codeStr: 'PH'; Alpha3code: iso3166_PHL; Alpha3codeStr: 'PHL'),
    (Code: 612; ShortName: 'Pitcairn'; Alpha2code: iso3166_PN; Alpha2codeStr: 'PN'; Alpha3code: iso3166_PCN; Alpha3codeStr: 'PCN'),
    (Code: 616; ShortName: 'Poland'; Alpha2code: iso3166_PL; Alpha2codeStr: 'PL'; Alpha3code: iso3166_POL; Alpha3codeStr: 'POL'),
    (Code: 620; ShortName: 'Portugal'; Alpha2code: iso3166_PT; Alpha2codeStr: 'PT'; Alpha3code: iso3166_PRT; Alpha3codeStr: 'PRT'),
    (Code: 630; ShortName: 'Puerto Rico'; Alpha2code: iso3166_PR; Alpha2codeStr: 'PR'; Alpha3code: iso3166_PRI; Alpha3codeStr: 'PRI'),
    (Code: 634; ShortName: 'Qatar'; Alpha2code: iso3166_QA; Alpha2codeStr: 'QA'; Alpha3code: iso3166_QAT; Alpha3codeStr: 'QAT'),
    (Code: 638; ShortName: 'Reunion'; Alpha2code: iso3166_RE; Alpha2codeStr: 'RE'; Alpha3code: iso3166_REU; Alpha3codeStr: 'REU'),
    (Code: 642; ShortName: 'Romania'; Alpha2code: iso3166_RO; Alpha2codeStr: 'RO'; Alpha3code: iso3166_ROU; Alpha3codeStr: 'ROU'),
    (Code: 643; ShortName: 'Russian Federation (the)'; Alpha2code: iso3166_RU; Alpha2codeStr: 'RU'; Alpha3code: iso3166_RUS; Alpha3codeStr: 'RUS'),
    (Code: 646; ShortName: 'Rwanda'; Alpha2code: iso3166_RW; Alpha2codeStr: 'RW'; Alpha3code: iso3166_RWA; Alpha3codeStr: 'RWA'),
    (Code: 652; ShortName: 'Saint Barthelemy'; Alpha2code: iso3166_BL; Alpha2codeStr: 'BL'; Alpha3code: iso3166_BLM; Alpha3codeStr: 'BLM'),
    (Code: 654; ShortName: 'Saint Helena, Ascension and Tristan da Cunha'; Alpha2code: iso3166_SH; Alpha2codeStr: 'SH'; Alpha3code: iso3166_SHN; Alpha3codeStr: 'SHN'),
    (Code: 659; ShortName: 'Saint Kitts and Nevis'; Alpha2code: iso3166_KN; Alpha2codeStr: 'KN'; Alpha3code: iso3166_KNA; Alpha3codeStr: 'KNA'),
    (Code: 662; ShortName: 'Saint Lucia'; Alpha2code: iso3166_LC; Alpha2codeStr: 'LC'; Alpha3code: iso3166_LCA; Alpha3codeStr: 'LCA'),
    (Code: 663; ShortName: 'Saint Martin (French part)'; Alpha2code: iso3166_MF; Alpha2codeStr: 'MF'; Alpha3code: iso3166_MAF; Alpha3codeStr: 'MAF'),
    (Code: 666; ShortName: 'Saint Pierre and Miquelon'; Alpha2code: iso3166_PM; Alpha2codeStr: 'PM'; Alpha3code: iso3166_SPM; Alpha3codeStr: 'SPM'),
    (Code: 670; ShortName: 'Saint Vincent and the Grenadines'; Alpha2code: iso3166_VC; Alpha2codeStr: 'VC'; Alpha3code: iso3166_VCT; Alpha3codeStr: 'VCT'),
    (Code: 882; ShortName: 'Samoa'; Alpha2code: iso3166_WS; Alpha2codeStr: 'WS'; Alpha3code: iso3166_WSM; Alpha3codeStr: 'WSM'),
    (Code: 674; ShortName: 'San Marino'; Alpha2code: iso3166_SM; Alpha2codeStr: 'SM'; Alpha3code: iso3166_SMR; Alpha3codeStr: 'SMR'),
    (Code: 678; ShortName: 'Sao Tome and Principe'; Alpha2code: iso3166_ST; Alpha2codeStr: 'ST'; Alpha3code: iso3166_STP; Alpha3codeStr: 'STP'),
    (Code: 682; ShortName: 'Saudi Arabia'; Alpha2code: iso3166_SA; Alpha2codeStr: 'SA'; Alpha3code: iso3166_SAU; Alpha3codeStr: 'SAU'),
    (Code: 686; ShortName: 'Senegal'; Alpha2code: iso3166_SN; Alpha2codeStr: 'SN'; Alpha3code: iso3166_SEN; Alpha3codeStr: 'SEN'),
    (Code: 688; ShortName: 'Serbia'; Alpha2code: iso3166_RS; Alpha2codeStr: 'RS'; Alpha3code: iso3166_SRB; Alpha3codeStr: 'SRB'),
    (Code: 690; ShortName: 'Seychelles'; Alpha2code: iso3166_SC; Alpha2codeStr: 'SC'; Alpha3code: iso3166_SYC; Alpha3codeStr: 'SYC'),
    (Code: 694; ShortName: 'Sierra Leone'; Alpha2code: iso3166_SL; Alpha2codeStr: 'SL'; Alpha3code: iso3166_SLE; Alpha3codeStr: 'SLE'),
    (Code: 702; ShortName: 'Singapore'; Alpha2code: iso3166_SG; Alpha2codeStr: 'SG'; Alpha3code: iso3166_SGP; Alpha3codeStr: 'SGP'),
    (Code: 534; ShortName: 'Sint Maarten (Dutch part)'; Alpha2code: iso3166_SX; Alpha2codeStr: 'SX'; Alpha3code: iso3166_SXM; Alpha3codeStr: 'SXM'),
    (Code: 703; ShortName: 'Slovakia'; Alpha2code: iso3166_SK; Alpha2codeStr: 'SK'; Alpha3code: iso3166_SVK; Alpha3codeStr: 'SVK'),
    (Code: 705; ShortName: 'Slovenia'; Alpha2code: iso3166_SI; Alpha2codeStr: 'SI'; Alpha3code: iso3166_SVN; Alpha3codeStr: 'SVN'),
    (Code: 90; ShortName: 'Solomon Islands'; Alpha2code: iso3166_SB; Alpha2codeStr: 'SB'; Alpha3code: iso3166_SLB; Alpha3codeStr: 'SLB'),
    (Code: 706; ShortName: 'Somalia'; Alpha2code: iso3166_SO; Alpha2codeStr: 'SO'; Alpha3code: iso3166_SOM; Alpha3codeStr: 'SOM'),
    (Code: 710; ShortName: 'South Africa'; Alpha2code: iso3166_ZA; Alpha2codeStr: 'ZA'; Alpha3code: iso3166_ZAF; Alpha3codeStr: 'ZAF'),
    (Code: 239; ShortName: 'South Georgia and the South Sandwich Islands'; Alpha2code: iso3166_GS; Alpha2codeStr: 'GS'; Alpha3code: iso3166_SGS; Alpha3codeStr: 'SGS'),
    (Code: 728; ShortName: 'South Sudan'; Alpha2code: iso3166_SS; Alpha2codeStr: 'SS'; Alpha3code: iso3166_SSD; Alpha3codeStr: 'SSD'),
    (Code: 724; ShortName: 'Spain'; Alpha2code: iso3166_ES; Alpha2codeStr: 'ES'; Alpha3code: iso3166_ESP; Alpha3codeStr: 'ESP'),
    (Code: 144; ShortName: 'Sri Lanka'; Alpha2code: iso3166_LK; Alpha2codeStr: 'LK'; Alpha3code: iso3166_LKA; Alpha3codeStr: 'LKA'),
    (Code: 729; ShortName: 'Sudan (the)'; Alpha2code: iso3166_SD; Alpha2codeStr: 'SD'; Alpha3code: iso3166_SDN; Alpha3codeStr: 'SDN'),
    (Code: 740; ShortName: 'Suriname'; Alpha2code: iso3166_SR; Alpha2codeStr: 'SR'; Alpha3code: iso3166_SUR; Alpha3codeStr: 'SUR'),
    (Code: 744; ShortName: 'Svalbard and Jan Mayen'; Alpha2code: iso3166_SJ; Alpha2codeStr: 'SJ'; Alpha3code: iso3166_SJM; Alpha3codeStr: 'SJM'),
    (Code: 752; ShortName: 'Sweden'; Alpha2code: iso3166_SE; Alpha2codeStr: 'SE'; Alpha3code: iso3166_SWE; Alpha3codeStr: 'SWE'),
    (Code: 756; ShortName: 'Switzerland'; Alpha2code: iso3166_CH; Alpha2codeStr: 'CH'; Alpha3code: iso3166_CHE; Alpha3codeStr: 'CHE'),
    (Code: 760; ShortName: 'Syrian Arab Republic'; Alpha2code: iso3166_SY; Alpha2codeStr: 'SY'; Alpha3code: iso3166_SYR; Alpha3codeStr: 'SYR'),
    (Code: 158; ShortName: 'Taiwan (Province of China)'; Alpha2code: iso3166_TW; Alpha2codeStr: 'TW'; Alpha3code: iso3166_TWN; Alpha3codeStr: 'TWN'),
    (Code: 762; ShortName: 'Tajikistan'; Alpha2code: iso3166_TJ; Alpha2codeStr: 'TJ'; Alpha3code: iso3166_TJK; Alpha3codeStr: 'TJK'),
    (Code: 834; ShortName: 'Tanzania, United Republic of'; Alpha2code: iso3166_TZ; Alpha2codeStr: 'TZ'; Alpha3code: iso3166_TZA; Alpha3codeStr: 'TZA'),
    (Code: 764; ShortName: 'Thailand'; Alpha2code: iso3166_TH; Alpha2codeStr: 'TH'; Alpha3code: iso3166_THA; Alpha3codeStr: 'THA'),
    (Code: 626; ShortName: 'Timor-Leste'; Alpha2code: iso3166_TL; Alpha2codeStr: 'TL'; Alpha3code: iso3166_TLS; Alpha3codeStr: 'TLS'),
    (Code: 768; ShortName: 'Togo'; Alpha2code: iso3166_TG; Alpha2codeStr: 'TG'; Alpha3code: iso3166_TGO; Alpha3codeStr: 'TGO'),
    (Code: 772; ShortName: 'Tokelau'; Alpha2code: iso3166_TK; Alpha2codeStr: 'TK'; Alpha3code: iso3166_TKL; Alpha3codeStr: 'TKL'),
    (Code: 776; ShortName: 'Tonga'; Alpha2code: iso3166_TO; Alpha2codeStr: 'TO'; Alpha3code: iso3166_TON; Alpha3codeStr: 'TON'),
    (Code: 780; ShortName: 'Trinidad and Tobago'; Alpha2code: iso3166_TT; Alpha2codeStr: 'TT'; Alpha3code: iso3166_TTO; Alpha3codeStr: 'TTO'),
    (Code: 788; ShortName: 'Tunisia'; Alpha2code: iso3166_TN; Alpha2codeStr: 'TN'; Alpha3code: iso3166_TUN; Alpha3codeStr: 'TUN'),
    (Code: 792; ShortName: 'Turkey'; Alpha2code: iso3166_TR; Alpha2codeStr: 'TR'; Alpha3code: iso3166_TUR; Alpha3codeStr: 'TUR'),
    (Code: 795; ShortName: 'Turkmenistan'; Alpha2code: iso3166_TM; Alpha2codeStr: 'TM'; Alpha3code: iso3166_TKM; Alpha3codeStr: 'TKM'),
    (Code: 796; ShortName: 'Turks and Caicos Islands (the)'; Alpha2code: iso3166_TC; Alpha2codeStr: 'TC'; Alpha3code: iso3166_TCA; Alpha3codeStr: 'TCA'),
    (Code: 798; ShortName: 'Tuvalu'; Alpha2code: iso3166_TV; Alpha2codeStr: 'TV'; Alpha3code: iso3166_TUV; Alpha3codeStr: 'TUV'),
    (Code: 800; ShortName: 'Uganda'; Alpha2code: iso3166_UG; Alpha2codeStr: 'UG'; Alpha3code: iso3166_UGA; Alpha3codeStr: 'UGA'),
    (Code: 804; ShortName: 'Ukraine'; Alpha2code: iso3166_UA; Alpha2codeStr: 'UA'; Alpha3code: iso3166_UKR; Alpha3codeStr: 'UKR'),
    (Code: 784; ShortName: 'United Arab Emirates (the)'; Alpha2code: iso3166_AE; Alpha2codeStr: 'AE'; Alpha3code: iso3166_ARE; Alpha3codeStr: 'ARE'),
    (Code: 826; ShortName: 'United Kingdom of Great Britain and Northern Ireland (the)'; Alpha2code: iso3166_GB; Alpha2codeStr: 'GB'; Alpha3code: iso3166_GBR; Alpha3codeStr: 'GBR'),
    (Code: 581; ShortName: 'United States Minor Outlying Islands (the)'; Alpha2code: iso3166_UM; Alpha2codeStr: 'UM'; Alpha3code: iso3166_UMI; Alpha3codeStr: 'UMI'),
    (Code: 840; ShortName: 'United States of America (the)'; Alpha2code: iso3166_US; Alpha2codeStr: 'US'; Alpha3code: iso3166_USA; Alpha3codeStr: 'USA'),
    (Code: 858; ShortName: 'Uruguay'; Alpha2code: iso3166_UY; Alpha2codeStr: 'UY'; Alpha3code: iso3166_URY; Alpha3codeStr: 'URY'),
    (Code: 860; ShortName: 'Uzbekistan'; Alpha2code: iso3166_UZ; Alpha2codeStr: 'UZ'; Alpha3code: iso3166_UZB; Alpha3codeStr: 'UZB'),
    (Code: 548; ShortName: 'Vanuatu'; Alpha2code: iso3166_VU; Alpha2codeStr: 'VU'; Alpha3code: iso3166_VUT; Alpha3codeStr: 'VUT'),
    (Code: 862; ShortName: 'Venezuela (Bolivarian Republic of)'; Alpha2code: iso3166_VE; Alpha2codeStr: 'VE'; Alpha3code: iso3166_VEN; Alpha3codeStr: 'VEN'),
    (Code: 704; ShortName: 'Viet Nam'; Alpha2code: iso3166_VN; Alpha2codeStr: 'VN'; Alpha3code: iso3166_VNM; Alpha3codeStr: 'VNM'),
    (Code: 92; ShortName: 'Virgin Islands (British)'; Alpha2code: iso3166_VG; Alpha2codeStr: 'VG'; Alpha3code: iso3166_VGB; Alpha3codeStr: 'VGB'),
    (Code: 850; ShortName: 'Virgin Islands (U.S.)'; Alpha2code: iso3166_VI; Alpha2codeStr: 'VI'; Alpha3code: iso3166_VIR; Alpha3codeStr: 'VIR'),
    (Code: 876; ShortName: 'Wallis and Futuna'; Alpha2code: iso3166_WF; Alpha2codeStr: 'WF'; Alpha3code: iso3166_WLF; Alpha3codeStr: 'WLF'),
    (Code: 732; ShortName: 'Western Sahara*'; Alpha2code: iso3166_EH; Alpha2codeStr: 'EH'; Alpha3code: iso3166_ESH; Alpha3codeStr: 'ESH'),
    (Code: 887; ShortName: 'Yemen'; Alpha2code: iso3166_YE; Alpha2codeStr: 'YE'; Alpha3code: iso3166_YEM; Alpha3codeStr: 'YEM'),
    (Code: 894; ShortName: 'Zambia'; Alpha2code: iso3166_ZM; Alpha2codeStr: 'ZM'; Alpha3code: iso3166_ZMB; Alpha3codeStr: 'ZMB'),
    (Code: 716; ShortName: 'Zimbabwe'; Alpha2code: iso3166_ZW; Alpha2codeStr: 'ZW'; Alpha3code: iso3166_ZWE; Alpha3codeStr: 'ZWE')
  );

  function ISO3166_byCode(ACode: word): TISO3166;
  function ISO3166_byAlpha2code(ACode: TISO3166_Alpha2code): TISO3166;
  function ISO3166_byAlpha2StrCode(ACode: String): TISO3166;
  function ISO3166_byAlpha3code(ACode: TISO3166_Alpha3code): TISO3166;
  function ISO3166_indexByCode(ACode: word): Integer;

implementation

uses SysUtils;

function ISO3166_indexByCode(ACode: word): Integer;
var i: integer;
begin
  Result:=0;
  for I := 1 to Length(ISO3166List)-1 do
    if ISO3166List[i].Code=ACode then begin
      Result:=i;
      exit;
    end;
end;

function ISO3166_byCode(ACode: word): TISO3166;
var i: integer;
begin
  Result:=ISO3166List[0];
  for I := 1 to Length(ISO3166List)-1 do
    if ISO3166List[i].Code=ACode then begin
      Result:=ISO3166List[i];
      exit;
    end;
end;

function ISO3166_byAlpha2code(ACode: TISO3166_Alpha2code): TISO3166;
var i: integer;
begin
  Result:=ISO3166List[0];
  for I := 1 to Length(ISO3166List)-1 do
    if ISO3166List[i].Alpha2code=ACode then begin
      Result:=ISO3166List[i];
      exit;
    end;
end;

function ISO3166_byAlpha2StrCode(ACode: String): TISO3166;
var i: integer;
begin
  Result:=ISO3166List[0];
  for I := 1 to Length(ISO3166List)-1 do
    if SameText(String(ISO3166List[i].Alpha2codeStr),ACode) then begin
      Result:=ISO3166List[i];
      exit;
    end;
end;

function ISO3166_byAlpha3code(ACode: TISO3166_Alpha3code): TISO3166;
var i: integer;
begin
  Result:=ISO3166List[0];
  for I := 1 to Length(ISO3166List)-1 do
    if ISO3166List[i].Alpha3code=ACode then begin
      Result:=ISO3166List[i];
      exit;
    end;
end;

end.
