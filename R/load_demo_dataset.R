#' Template for new functions
#'
#' This is a template.
#' @param my_param Yep, it's a parameter.
#' @return It returns...
#' @examples
#' Some examples
#' @section Warning:
#' This is a warning
#' @export
load_demo_dataset = function(dataset = NULL) {
  datasets = c("EGFR", "FAS", "QUORUM", "TEMPLATE")
  if (is.null(dataset)) {
    stop("Expected a dataset from the following list: ", paste(datasets, collapse = ", "))
  }

  ##################################################
  ################# BEGINNING OF ###################
  ################# EGFR ###########################
  ##################################################
  if (dataset == "EGFR") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }
  ##################################################
  ################# END OF #########################
  ################# EGFR ###########################
  ##################################################




  ##################################################
  ################# BEGINNING OF ###################
  ################# FAS ############################
  ##################################################
  if (dataset == "FAS") {
    cat(crayon::bold("Returning fas_rap"))
    cat("\nBased on", crayon::italic("Simulating FAS-Induced Apoptosis by Using P Systems"), "by Smitha Cheruku, Andrei Păun, Francisco J. Romero-Campero, Mario J. Pérez-Jiménez, and Oscar H. Ibarra")

    n_rules = 99

    fas_rap = list(

      "Configuration" = tibble::tibble(
        environment = c("e", "e", "e", "e"),
        id = c("e", "s", "c", "m"),
        label = c("e", "s", "c", "m"),
        objects = list(
          # e
          tibble::tibble(object = "FASL", multiplicity = 12500),
          # s
          tibble::tibble(object = "FASR",
                         multiplicity = 6023),
          # c
          tibble::tibble(object = c("FADD", "CASP8", "FLIP", "CASP3", "Bid", "Bax", "XIAP", "Apaf", "CASP9"),
                         multiplicity = c(10040, 20074, 48786, 120460, 15057, 50189, 18069, 60230, 12046)),
          # m
          tibble::tibble(object = c("Smac", "Cyto.c", "Bcl2"),
                         multiplicity = c(60230, 60230, 45172))
        ),
        superM = c(NA, "e", "s", "c"), # Given by ID # END: We could have more than one parent

        subM = list(
          tibble::tibble(children = "s"),
          tibble::tibble(children = "c"),
          tibble::tibble(children = "m"),
          tibble::tibble(children = NA)),
        charge = c("-", "-", "-", "-"),
        other_params = c(NA, NA, NA, NA)
      ),

      "Rules" = tibble::tibble(
        rule_id = 1:n_rules,
        dissolves = c(rep(FALSE, n_rules)),
        priority = rep("-", n_rules),

        main_membrane_label = c(
        # r1 : FASL[ FASR ]s → [ FASC ]s k1f
        "e",
        # r2 : [ FASC ]s → FASL[ FASC ]s k1r
        "e",
        # r3 : FASC[ FADD ]c → FASC : FADD[ ]c k2f
        "s",
        # r4 : FASC : FADD[ ]c → FASC[ FADD ]c k2r
        "s",
        # r5 : FASC : FADD[ FADD ]c → FASC : FADD2[ ]c k2f
        "s",
        # r6 : FASC : FADD2[ ]c → FASC : FADD[ FADD ]c k2r
        "s",
        # r7 : FASC : FADD2[ FADD ]c → FASC : FADD3[ ]c k2f
        "s",
        # r8 : FASC : FADD3[ ]c → FASC : FADD2[ FADD ]c k2r
        "s",
        # r9 : FASC : FADD2 : CASP8[ FADD ]c → FASC : FADD3 : CASP8[ ]c k2f
        "s",
        # r10 : FASC : FADD3 : CASP8[ ]c → FASC : FADD2 : CASP8[ FADD ]c k2r
        "s",
        # r11 : FASC : FADD2 : FLIP[ FADD ]c → FASC : FADD3 : FLIP[ ]c k2f
        "s",
        # r12 : FASC : FADD3 : FLIP[ ]c → FASC : FADD2 : FLIP[ FADD ]c k2r
        "s",
        # r13 : FASC : FADD2 : CASP82[ FADD ]c → FASC : FADD3 : CASP82[ ]c k2f
        "s",
        # r14 : FASC : FADD3 : CASP82[ ]c → FASC : FADD2 : CASP82[ FADD ]c k2r
        "s",
        # r15 : FASC : FADD2 : CASP8 : FLIP[ FADD ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k2f
        "s",
        # r16 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD2 : CASP8 : FLIP[ FADD ]c k2r
        "s",
        # r17 : FASC : FADD2 : FLIP2[ FADD ]c → FASC : FADD3 : FLIP2[ ]c k2f
        "s",
        # r18 : FASC : FADD3 : FLIP2[ ]c → FASC : FADD2 : FLIP2[ FADD ]c k2r
        "s",
        # r19 : FASC : FADD : CASP8[ FADD ]c → FASC : FADD2 : CASP8[ ]c k2f
        "s",
        # r20 : FASC : FADD2 : CASP8[ ]c → FASC : FADD : CASP8[ FADD ]c k2r
        "s",
        # r21 : FASC : FADD : FLIP[ FADD ]c → FASC : FADD2 : FLIP[ ]c k2f
        "s",
        # r22 : FASC : FADD2 : FLIP[ ]c → FASC : FADD : FLIP[ FADD ]c k2r
        "s",
        # r23 : FASC : FADD3[ CASP8 ]c → FASC : FADD3 : CASP8[ ]c k2f
        "s",
        # r24 : FASC : FADD3 : CASP8[ ]c → FASC : FADD3[ CASP8 ]c k2r
        "s",
        # r25 : FASC : FADD3[ FLIP ]c → FASC : FADD3 : FLIP[ ]c k3f
        "s",
        # r26 : FASC : FADD3 : FLIP[ ]c → FASC : FADD3[ FLIP ]c k3r
        "s",
        # r27 : FASC : FADD3 : CASP8[ CASP8 ]c → FASC : FADD3 : CASP82[ ]c k3f
        "s",
        # r28 : FASC : FADD3 : CASP82[ ]c → FASC : FADD3 : CASP8[ CASP8 ]c k3r
        "s",
        # r29 : FASC : FADD3 : CASP8[ FLIP ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k3f
        "s",
        # r30 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD3 : CASP8[ FLIP ]c k3r
        "s",
        # r31 : FASC : FADD3 : FLIP[ CASP8 ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k3f
        "s",
        # r32 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD3 : FLIP[ CASP8 ]c k3r
        "s",
        # r33 : FASC : FADD3 : FLIP[ FLIP ]c → FASC : FADD3 : FLIP2[ ]c k3f
        "s",
        # r34 : FASC : FADD3 : FLIP2[ ]c → FASC : FADD3 : FLIP[ FLIP ]c k3r
        "s",
        # r35 : FASC : FADD3 : CASP82[ CASP8 ]c → FASC : FADD3 : CASP83[ ]c k3f
        "s",
        # r36 : FASC : FADD3 : CASP83[ ]c → FASC : FADD3 : CASP82[ CASP8 ]c k3r
        "s",
        # r37 : FASC : FADD3 : CASP82[ FLIP ]c → FASC : FADD3 : CASP82 : FLIP[ ]c k3f
        "s",
        # r38 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : CASP82[ FLIP ]c k3r
        "s",
        # r39 : FASC : FADD3 : CASP8 : FLIP[ CASP8 ]c → FASC : FADD3 : CASP82 : FLIP[ ]c k3f
        "s",
        # r40 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : CASP8 : FLIP[ CASP8 ]c k3r
        "s",
        # r41 : FASC : FADD3 : CASP8 : FLIP[ FLIP ]c → FASC : FADD3 : CASP8 : FLIP2[ ]c k3f
        "s",
        # r42 : FASC : FADD3 : CASP8 : FLIP2[ ]c → FASC : FADD3 : CASP8 : FLIP[ FLIP ]c k3r
        "s",
        # r43 : FASC : FADD3 : FLIP2[ CASP8 ]c → FASC : FADD3 : CASP8 : FLIP2[ ]c k3f
        "s",
        # r44 : FASC : FADD3 : CASP8 : FLIP2[ ]c → FASC : FADD3 : FLIP2[ CASP8 ]c k3r
        "s",
        # r45 : FASC : FADD3 : FLIP2[ FLIP ]c → FASC : FADD3 : FLIP3[ ]c k3f
        "s",
        # r46 : FASC : FADD3 : FLIP3[ ]c → FASC : FADD3 : FLIP2[ FLIP ]c k3r
        "s",
        # r47 : FASC : FADD2[ CASP8 ]c → FASC : FADD2 : CASP8[ ]c k3f
        "s",
        # r48 : FASC : FADD2 : CASP8[ ]c → FASC : FADD2[ CASP8 ]c k3r
        "s",
        # r49 : FASC : FADD2[ FLIP ]c → FASC : FADD2 : FLIP[ ]c k3f
        "s",
        # r50 : FASC : FADD2 : FLIP[ ]c → FASC : FADD2[ FLIP ]c k3r
        "s",
        # r51 : FASC : FADD2 : CASP8[ CASP8 ]c → FASC : FADD2 : CASP82[ ]c k3f
        "s",
        # r52 : FASC : FADD2 : CASP82[ ]c → FASC : FADD2 : CASP8[ CASP8 ]c k3r
        "s",
        # r53 : FASC : FADD2 : CASP8[ FLIP ]c → FASC : FADD2 : CASP8 : FLIP[ ]c k3f
        "s",
        # r54 : FASC : FADD2 : CASP8 : FLIP[ ]c → FASC : FADD2 : CASP8[ FLIP ]c k3r
        "s",
        # r55 : FASC : FADD2 : FLIP[ CASP8 ]c → FASC : FADD2 : CASP8 : FLIP[ ]c k3f
        "s",
        # r56 : FASC : FADD2 : CASP8 : FLIP[ ]c → FASC : FADD2 : FLIP[ CASP8 ]c k3r
        "s",
        # r57 : FASC : FADD2 : FLIP[ FLIP ]c → FASC : FADD2 : FLIP2[ ]c k3f
        "s",
        # r58 : FASC : FADD2 : FLIP2[ ]c → FASC : FADD2 : FLIP[ FLIP ]c k3r
        "s",
        # r59 : FASC : FADD[ CASP8 ]c → FASC : FADD : CASP8[ ]c k3f
        "s",
        # r60 : FASC : FADD : CASP8[ ]c → FASC : FADD[ CASP8 ]c k3r
        "s",
        # r61 : FASC : FADD[ FLIP ]c → FASC : FADD : FLIP[ ]c k3f
        "s",
        # r62 : FASC : FADD : FLIP[ ]c → FASC : FADD[ FLIP ]c k3r
        "s",
        # r63 : FASC : FADD2 : CASP82[ ]c → FASC : FADD2[ CASP8P41 2 ]c k4
        "s",
        # r64 : FASC : FADD3 : CASP83[ ]c → FASC : FADD3 : CASP8[ CASP8P41 2 ]c k4
        "s",
        # r65 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : FLIP[ CASP8P41 2 ]c k4
        "s",
        # r66 : FASC : FADD3 : CASP82[ ]c → FASC : FADD3[ CASP8P41 2 ]c k4
        "s",
        # EVOLUTION
        # r67 : [ CASP8P41 2 ]c → [ CASP8∗2 ]c k5
        "c",
        # r68 : [ CASP8∗2 , CASP3 ]c → [ CASP8∗2 : CASP3 ]c k6f
        "c",
        # r69 : [ CASP8∗2 : CASP3 ]c → [ CASP8∗2 , CASP3 ]c k6r
        "c",
        # r70 : [ CASP8∗2 , CASP3∗ ]c → [ CASP8∗2 : CASP3 ]c k7
        "c",
        # r71 : [ CASP8∗2, Bid ]c → [ CASP8∗2: Bid ]c k8f
        "c",
        # r72 : [ CASP8∗2: Bid ]c → [ CASP8∗2, Bid ]c k8r
        "c",
        # r73 : [ CASP8∗2, tBid ]c → [ CASP8∗2: Bid ]c k7
        "c",
        # r74 : [ tBid, Bax ]c → [ tBid : Bax ]c k9f
        "c",
        # r75 : [ tBid : Bax ]c → [ tBid, Bax ]c k9r
        "c",
        # r76 : [ tBid : Bax, Bax ]c → [ tBid : Bax2 ]c k9f
        "c",
        # r77 : [ tBid : Bax2 ]c → [ tBid : Bax, Bax ]c k9r
        "c",
        ## m
        # r78 : tBid : Bax2[ Smac ]m → Smac∗[ ]m k10
        "c",
        # r79 : tBid : Bax2[ Cyto.c ]m → Cyto.c∗[ ]m k10
        "c",
        ## STANDARD
        # r80 : [ Smac∗, XIAP ]c → [ Smac∗ : XIAP ]c k11f
        "s",
        # r81 : [ Smac∗ : XIAP ]c → [ Smac∗, XIAP ]c k11r
        "s",
        # r82 : [ Cyto.c∗, Apaf ]c → [ Cyto.c∗ : Apaf : ATP ]c k12f
        "s",
        # r83 : [ Cyto.c∗ : Apaf : ATP ]c → [ Cyto.c∗, Apaf ]c k12r
        "s",
        # r84 : [ Cyto.c∗ : Apaf : ATP, CASP9 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9 ]c k13f
        "s",
        # r85 : [ Cyto.c∗ : Apaf : ATP : CASP9 ]c → [ Cyto.c∗ : Apaf : ATP, CASP9 ]c k13r
        "s",
        # r86 : [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9 ]c → [ Cyto.c∗ : Apaf : ATP : CASP92 ]c k14f
        "s",
        # r87 : [ Cyto.c∗ : Apaf : ATP : CASP92 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9 ]c k14r
        "s",
        # r88 : [ Cyto.c∗ : Apaf : ATP : CASP92 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9∗ ]c k15
        "s",
        # r89 : [ CASP9∗, CASP3 ]c → [ CASP9∗ : CASP3 ]c k16f
        "s",
        # r90 : [ CASP9∗ : CASP3 ]c → [ CASP9∗, CASP3 ]c k16r
        "s",
        # r91 : [ CASP9∗ : CASP3 ]c → [ CASP9∗, CASP3∗ ]c k17
        "s",
        # r92 : [ CASP9, XIAP ]c → [ CASP9 : XIAP ]c k18f
        "s",
        # r93 : [ CASP9 : XIAP ]c → [ CASP9, XIAP ]c k18r
        "s",
        # r94 : [ CASP3∗, XIAP ]c → [ CASP3∗ : XIAP ]c k19f
        "s",
        # r95 : [ CASP3∗ : XIAP ]c → [ CASP3∗, XIAP ]c k19r
        "s",
        ## m
        # r96 : Bax[ Bcl2 ]m → [ Bcl2 : Bax ]m k20f
        "c",
        # r97 : [ Bcl2 : Bax ]m → Bax[ Bcl2 ]m k20r
        "c",
        # r960 : Bid[ Bcl2 ]m → [ Bcl2 : Bid ]m k20f
        "c",
        # r970 : [ Bcl2 : Bid ]m → Bid[ Bcl2 ]m k20r
        "c",
        # r9600 : tBid[ Bcl2 ]m → [ Bcl2 : tBid ]m k20f
        "c",
        # r9700 : [ Bcl2 : tBid ]m → tBid[ Bcl2 ]m k20r
        "c"
        ),

        lhs = list(
          # r1 : FASL[ FASR ]s → [ FASC ]s k1f
          tibble::tibble(where = c("@here", "s"),
                         object = c("FASL", "FASR"),
                         multiplicity = c(1, 1)),
          # r2 : [ FASC ]s → FASL[ FASC ]s k1r
          tibble::tibble(where = c("s"),
                         object = c("FASC"),
                         multiplicity = c(1)),
          # r3 : FASC[ FADD ]c → FASC : FADD[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC", "FADD"),
                         multiplicity = c(1, 1)),
          # r4 : FASC : FADD[ ]c → FASC[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD", "c"),
                         multiplicity = c(1, 1)),
          # r5 : FASC : FADD[ FADD ]c → FASC : FADD2[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD", "FADD"),
                         multiplicity = c(1, 1)),
          # r6 : FASC : FADD2[ ]c → FASC : FADD[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2", "c"),
                         multiplicity = c(1, 1)),
          # r7 : FASC : FADD2[ FADD ]c → FASC : FADD3[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2", "FADD"),
                         multiplicity = c(1, 1)),
          # r8 : FASC : FADD3[ ]c → FASC : FADD2[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3", "c"),
                         multiplicity = c(1, 1)),
          # r9 : FASC : FADD2 : CASP8[ FADD ]c → FASC : FADD3 : CASP8[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8", "FADD"),
                         multiplicity = c(1, 1)),
          # r10 : FASC : FADD3 : CASP8[ ]c → FASC : FADD2 : CASP8[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r11 : FASC : FADD2 : FLIP[ FADD ]c → FASC : FADD3 : FLIP[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP", "FADD"),
                         multiplicity = c(1, 1)),
          # r12 : FASC : FADD3 : FLIP[ ]c → FASC : FADD2 : FLIP[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r13 : FASC : FADD2 : CASP82[ FADD ]c → FASC : FADD3 : CASP82[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP82", "FADD"),
                         multiplicity = c(1, 1)),
          # r14 : FASC : FADD3 : CASP82[ ]c → FASC : FADD2 : CASP82[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r15 : FASC : FADD2 : CASP8 : FLIP[ FADD ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8:FLIP", "FADD"),
                         multiplicity = c(1, 1)),
          # r16 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD2 : CASP8 : FLIP[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r17 : FASC : FADD2 : FLIP2[ FADD ]c → FASC : FADD3 : FLIP2[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP2", "FADD"),
                         multiplicity = c(1, 1)),
          # r18 : FASC : FADD3 : FLIP2[ ]c → FASC : FADD2 : FLIP2[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r19 : FASC : FADD : CASP8[ FADD ]c → FASC : FADD2 : CASP8[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD:CASP8", "FADD"),
                         multiplicity = c(1, 1)),
          # r20 : FASC : FADD2 : CASP8[ ]c → FASC : FADD : CASP8[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r21 : FASC : FADD : FLIP[ FADD ]c → FASC : FADD2 : FLIP[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD:FLIP", "FADD"),
                         multiplicity = c(1, 1)),
          # r22 : FASC : FADD2 : FLIP[ ]c → FASC : FADD : FLIP[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r23 : FASC : FADD3[ CASP8 ]c → FASC : FADD3 : CASP8[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3", "CASP8"),
                         multiplicity = c(1, 1)),
          # r24 : FASC : FADD3 : CASP8[ ]c → FASC : FADD3[ CASP8 ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r25 : FASC : FADD3[ FLIP ]c → FASC : FADD3 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3", "FLIP"),
                         multiplicity = c(1, 1)),
          # r26 : FASC : FADD3 : FLIP[ ]c → FASC : FADD3[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r27 : FASC : FADD3 : CASP8[ CASP8 ]c → FASC : FADD3 : CASP82[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP8", "CASP8"),
                         multiplicity = c(1, 1)),
          # r28 : FASC : FADD3 : CASP82[ ]c → FASC : FADD3 : CASP8[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r29 : FASC : FADD3 : CASP8[ FLIP ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP8", "FLIP"),
                         multiplicity = c(1, 1)),
          # r30 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD3 : CASP8[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r31 : FASC : FADD3 : FLIP[ CASP8 ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP", "CASP8"),
                         multiplicity = c(1, 1)),
          # r32 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD3 : FLIP[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r33 : FASC : FADD3 : FLIP[ FLIP ]c → FASC : FADD3 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP", "FLIP"),
                         multiplicity = c(1, 1)),
          # r34 : FASC : FADD3 : FLIP2[ ]c → FASC : FADD3 : FLIP[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r35 : FASC : FADD3 : CASP82[ CASP8 ]c → FASC : FADD3 : CASP83[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP82", "CASP82"),
                         multiplicity = c(1, 1)),
          # r36 : FASC : FADD3 : CASP83[ ]c → FASC : FADD3 : CASP82[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP83", "c"),
                         multiplicity = c(1, 1)),
          # r37 : FASC : FADD3 : CASP82[ FLIP ]c → FASC : FADD3 : CASP82 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP82", "FLIP"),
                         multiplicity = c(1, 1)),
          # r38 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : CASP82[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r39 : FASC : FADD3 : CASP8 : FLIP[ CASP8 ]c → FASC : FADD3 : CASP82 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP8:FLIP", "CASP8"),
                         multiplicity = c(1, 1)),
          # r40 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : CASP8 : FLIP[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r41 : FASC : FADD3 : CASP8 : FLIP[ FLIP ]c → FASC : FADD3 : CASP8 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP82:FLIP", "FLIP"),
                         multiplicity = c(1, 1)),
          # r42 : FASC : FADD3 : CASP8 : FLIP2[ ]c → FASC : FADD3 : CASP8 : FLIP[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r43 : FASC : FADD3 : FLIP2[ CASP8 ]c → FASC : FADD3 : CASP8 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP2", "CASP8"),
                         multiplicity = c(1, 1)),
          # r44 : FASC : FADD3 : CASP8 : FLIP2[ ]c → FASC : FADD3 : FLIP2[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r45 : FASC : FADD3 : FLIP2[ FLIP ]c → FASC : FADD3 : FLIP3[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP2", "FLIP"),
                         multiplicity = c(1, 1)),
          # r46 : FASC : FADD3 : FLIP3[ ]c → FASC : FADD3 : FLIP2[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP3", "c"),
                         multiplicity = c(1, 1)),
          # r47 : FASC : FADD2[ CASP8 ]c → FASC : FADD2 : CASP8[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2", "CASP8"),
                         multiplicity = c(1, 1)),
          # r48 : FASC : FADD2 : CASP8[ ]c → FASC : FADD2[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r49 : FASC : FADD2[ FLIP ]c → FASC : FADD2 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2", "FLIP"),
                         multiplicity = c(1, 1)),
          # r50 : FASC : FADD2 : FLIP[ ]c → FASC : FADD2[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r51 : FASC : FADD2 : CASP8[ CASP8 ]c → FASC : FADD2 : CASP82[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8", "CASP8"),
                         multiplicity = c(1, 1)),
          # r52 : FASC : FADD2 : CASP82[ ]c → FASC : FADD2 : CASP8[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r53 : FASC : FADD2 : CASP8[ FLIP ]c → FASC : FADD2 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8", "FLIP"),
                         multiplicity = c(1, 1)),
          # r54 : FASC : FADD2 : CASP8 : FLIP[ ]c → FASC : FADD2 : CASP8[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r55 : FASC : FADD2 : FLIP[ CASP8 ]c → FASC : FADD2 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP", "CASP8"),
                         multiplicity = c(1, 1)),
          # r56 : FASC : FADD2 : CASP8 : FLIP[ ]c → FASC : FADD2 : FLIP[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r57 : FASC : FADD2 : FLIP[ FLIP ]c → FASC : FADD2 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP", "FLIP"),
                         multiplicity = c(1, 1)),
          # r58 : FASC : FADD2 : FLIP2[ ]c → FASC : FADD2 : FLIP[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r59 : FASC : FADD[ CASP8 ]c → FASC : FADD : CASP8[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD", "CASP8"),
                         multiplicity = c(1, 1)),
          # r60 : FASC : FADD : CASP8[ ]c → FASC : FADD[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r61 : FASC : FADD[ FLIP ]c → FASC : FADD : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD", "FLIP"),
                         multiplicity = c(1, 1)),
          # r62 : FASC : FADD : FLIP[ ]c → FASC : FADD[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r63 : FASC : FADD2 : CASP82[ ]c → FASC : FADD2[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r64 : FASC : FADD3 : CASP83[ ]c → FASC : FADD3 : CASP8[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP83", "c"),
                         multiplicity = c(1, 1)),
          # r65 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : FLIP[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r66 : FASC : FADD3 : CASP82[ ]c → FASC : FADD3[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          ## EVOLUTION
          # r67 : [ CASP8P41 2 ]c → [ CASP8∗2 ]c k5
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r68 : [ CASP8∗2 , CASP3 ]c → [ CASP8∗2 : CASP3 ]c k6f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "CASP3"),
                         multiplicity = c(1, 1)),
          # r69 : [ CASP8∗2 : CASP3 ]c → [ CASP8∗2 , CASP3 ]c k6r
          tibble::tibble(where = c("@here"),
                         object = c("CASP8*2:CASP3"),
                         multiplicity = c(1)),
          # r70 : [ CASP8∗2 , CASP3∗ ]c → [ CASP8∗2 : CASP3 ]c k7
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "CASP3"),
                         multiplicity = c(1, 1)),
          # r71 : [ CASP8∗2, Bid ]c → [ CASP8∗2: Bid ]c k8f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "Bid"),
                         multiplicity = c(1, 1)),
          # r72 : [ CASP8∗2: Bid ]c → [ CASP8∗2, Bid ]c k8r
          tibble::tibble(where = c("@here"),
                         object = c("CASP8*2:Bid"),
                         multiplicity = c(1)),
          # r73 : [ CASP8∗2, tBid ]c → [ CASP8∗2: Bid ]c k7
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "tBid"),
                         multiplicity = c(1, 1)),
          # r74 : [ tBid, Bax ]c → [ tBid : Bax ]c k9f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("tBid", "Bax"),
                         multiplicity = c(1, 1)),
          # r75 : [ tBid : Bax ]c → [ tBid, Bax ]c k9r
          tibble::tibble(where = c("@here"),
                         object = c("tBid:Bax"),
                         multiplicity = c(1)),
          # r76 : [ tBid : Bax, Bax ]c → [ tBid : Bax2 ]c k9f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("tBid:Bax", "Bax"),
                         multiplicity = c(1, 1)),
          # r77 : [ tBid : Bax2 ]c → [ tBid : Bax, Bax ]c k9r
          tibble::tibble(where = c("@here"),
                         object = c("tBid:Bax2"),
                         multiplicity = c(1)),
          ## m
          # r78 : tBid : Bax2[ Smac ]m → Smac∗[ ]m k10
          tibble::tibble(where = c("@here", "m"),
                         object = c("tBid:Bax2", "Smac"),
                         multiplicity = c(1, 1)),
          # r79 : tBid : Bax2[ Cyto.c ]m → Cyto.c∗[ ]m k10
          tibble::tibble(where = c("@here", "m"),
                         object = c("tBid:Bax2", "Cyto.c"),
                         multiplicity = c(1, 1)),
          ## STANDARD
          # r80 : [ Smac∗, XIAP ]c → [ Smac∗ : XIAP ]c k11f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Smac*", "XIAP"),
                         multiplicity = c(1, 1)),
          # r81 : [ Smac∗ : XIAP ]c → [ Smac∗, XIAP ]c k11r
          tibble::tibble(where = c("@here"),
                         object = c("Smac:XIAP"),
                         multiplicity = c(1)),
          # r82 : [ Cyto.c∗, Apaf ]c → [ Cyto.c∗ : Apaf : ATP ]c k12f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Cyto.c*", "Apaf"),
                         multiplicity = c(1, 1)),
          # r83 : [ Cyto.c∗ : Apaf : ATP ]c → [ Cyto.c∗, Apaf ]c k12r
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP"),
                         multiplicity = c(1)),
          # r84 : [ Cyto.c∗ : Apaf : ATP, CASP9 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9 ]c k13f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Cyto.c*:Apaf:ATP", "CASP9"),
                         multiplicity = c(1, 1)),
          # r85 : [ Cyto.c∗ : Apaf : ATP : CASP9 ]c → [ Cyto.c∗ : Apaf : ATP, CASP9 ]c k13r
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP9"),
                         multiplicity = c(1)),
          # r86 : [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9 ]c → [ Cyto.c∗ : Apaf : ATP : CASP92 ]c k14f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP9", "CASP9"),
                         multiplicity = c(1, 1)),
          # r87 : [ Cyto.c∗ : Apaf : ATP : CASP92 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9 ]c k14r
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP92"),
                         multiplicity = c(1)),
          # r88 : [ Cyto.c∗ : Apaf : ATP : CASP92 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9∗ ]c k15
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP92"),
                         multiplicity = c(1)),
          # r89 : [ CASP9∗, CASP3 ]c → [ CASP9∗ : CASP3 ]c k16f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP9*", "CASP3"),
                         multiplicity = c(1, 1)),
          # r90 : [ CASP9∗ : CASP3 ]c → [ CASP9∗, CASP3 ]c k16r
          tibble::tibble(where = c("@here"),
                         object = c("CASP9*:CASP3"),
                         multiplicity = c(1)),
          # r91 : [ CASP9∗ : CASP3 ]c → [ CASP9∗, CASP3∗ ]c k17
          tibble::tibble(where = c("@here"),
                         object = c("CASP9*:CASP3"),
                         multiplicity = c(1)),
          # r92 : [ CASP9, XIAP ]c → [ CASP9 : XIAP ]c k18f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP9", "XIAP"),
                         multiplicity = c(1, 1)),
          # r93 : [ CASP9 : XIAP ]c → [ CASP9, XIAP ]c k18r
          tibble::tibble(where = c("@here"),
                         object = c("CASP9:XIAP"),
                         multiplicity = c(1)),
          # r94 : [ CASP3∗, XIAP ]c → [ CASP3∗ : XIAP ]c k19f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP3*", "XIAP"),
                         multiplicity = c(1, 1)),
          # r95 : [ CASP3∗ : XIAP ]c → [ CASP3∗, XIAP ]c k19r
          tibble::tibble(where = c("@here"),
                         object = c("CASP3*:XIAP"),
                         multiplicity = c(1)),
          ## m
          # r96 : Bax[ Bcl2 ]m → [ Bcl2 : Bax ]m k20f
          tibble::tibble(where = c("@here", "m"),
                         object = c("Bax", "Bcl2"),
                         multiplicity = c(1, 1)),
          # r97 : [ Bcl2 : Bax ]m → Bax[ Bcl2 ]m k20r
          tibble::tibble(where = c("@here"),
                         object = c("Bcl2:Bax"),
                         multiplicity = c(1)),
          # r960 : Bid[ Bcl2 ]m → [ Bcl2 : Bid ]m k20f
          tibble::tibble(where = c("@here", "m"),
                         object = c("Bid", "Bcl2"),
                         multiplicity = c(1, 1)),
          # r970 : [ Bcl2 : Bid ]m → Bid[ Bcl2 ]m k20r
          tibble::tibble(where = c("@here"),
                         object = c("Bcl2:Bid"),
                         multiplicity = c(1)),
          # r9600 : tBid[ Bcl2 ]m → [ Bcl2 : tBid ]m k20f
          tibble::tibble(where = c("@here", "m"),
                         object = c("tBid", "Bcl2"),
                         multiplicity = c(1, 1)),
          # r9700 : [ Bcl2 : tBid ]m → tBid[ Bcl2 ]m k20r
          tibble::tibble(where = c("@here"),
                         object = c("Bcl2:tBid"),
                         multiplicity = c(1))
        ),

        rhs = list(
          # TODO:

          # r1 : FASL[ FASR ]s → [ FASC ]s k1f
          tibble::tibble(where = c("@here", "s"),
                         object = c("FASL", "FASR"),
                         multiplicity = c(1, 1)),
          # r2 : [ FASC ]s → FASL[ FASC ]s k1r
          tibble::tibble(where = c("s"),
                         object = c("FASC"),
                         multiplicity = c(1)),
          # r3 : FASC[ FADD ]c → FASC : FADD[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC", "FADD"),
                         multiplicity = c(1, 1)),
          # r4 : FASC : FADD[ ]c → FASC[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD", "c"),
                         multiplicity = c(1, 1)),
          # r5 : FASC : FADD[ FADD ]c → FASC : FADD2[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD", "FADD"),
                         multiplicity = c(1, 1)),
          # r6 : FASC : FADD2[ ]c → FASC : FADD[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2", "c"),
                         multiplicity = c(1, 1)),
          # r7 : FASC : FADD2[ FADD ]c → FASC : FADD3[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2", "FADD"),
                         multiplicity = c(1, 1)),
          # r8 : FASC : FADD3[ ]c → FASC : FADD2[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3", "c"),
                         multiplicity = c(1, 1)),
          # r9 : FASC : FADD2 : CASP8[ FADD ]c → FASC : FADD3 : CASP8[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8", "FADD"),
                         multiplicity = c(1, 1)),
          # r10 : FASC : FADD3 : CASP8[ ]c → FASC : FADD2 : CASP8[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r11 : FASC : FADD2 : FLIP[ FADD ]c → FASC : FADD3 : FLIP[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP", "FADD"),
                         multiplicity = c(1, 1)),
          # r12 : FASC : FADD3 : FLIP[ ]c → FASC : FADD2 : FLIP[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r13 : FASC : FADD2 : CASP82[ FADD ]c → FASC : FADD3 : CASP82[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP82", "FADD"),
                         multiplicity = c(1, 1)),
          # r14 : FASC : FADD3 : CASP82[ ]c → FASC : FADD2 : CASP82[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r15 : FASC : FADD2 : CASP8 : FLIP[ FADD ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8:FLIP", "FADD"),
                         multiplicity = c(1, 1)),
          # r16 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD2 : CASP8 : FLIP[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r17 : FASC : FADD2 : FLIP2[ FADD ]c → FASC : FADD3 : FLIP2[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP2", "FADD"),
                         multiplicity = c(1, 1)),
          # r18 : FASC : FADD3 : FLIP2[ ]c → FASC : FADD2 : FLIP2[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r19 : FASC : FADD : CASP8[ FADD ]c → FASC : FADD2 : CASP8[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD:CASP8", "FADD"),
                         multiplicity = c(1, 1)),
          # r20 : FASC : FADD2 : CASP8[ ]c → FASC : FADD : CASP8[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r21 : FASC : FADD : FLIP[ FADD ]c → FASC : FADD2 : FLIP[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD:FLIP", "FADD"),
                         multiplicity = c(1, 1)),
          # r22 : FASC : FADD2 : FLIP[ ]c → FASC : FADD : FLIP[ FADD ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r23 : FASC : FADD3[ CASP8 ]c → FASC : FADD3 : CASP8[ ]c k2f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3", "CASP8"),
                         multiplicity = c(1, 1)),
          # r24 : FASC : FADD3 : CASP8[ ]c → FASC : FADD3[ CASP8 ]c k2r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r25 : FASC : FADD3[ FLIP ]c → FASC : FADD3 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3", "FLIP"),
                         multiplicity = c(1, 1)),
          # r26 : FASC : FADD3 : FLIP[ ]c → FASC : FADD3[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r27 : FASC : FADD3 : CASP8[ CASP8 ]c → FASC : FADD3 : CASP82[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP8", "CASP8"),
                         multiplicity = c(1, 1)),
          # r28 : FASC : FADD3 : CASP82[ ]c → FASC : FADD3 : CASP8[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r29 : FASC : FADD3 : CASP8[ FLIP ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP8", "FLIP"),
                         multiplicity = c(1, 1)),
          # r30 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD3 : CASP8[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r31 : FASC : FADD3 : FLIP[ CASP8 ]c → FASC : FADD3 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP", "CASP8"),
                         multiplicity = c(1, 1)),
          # r32 : FASC : FADD3 : CASP8 : FLIP[ ]c → FASC : FADD3 : FLIP[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r33 : FASC : FADD3 : FLIP[ FLIP ]c → FASC : FADD3 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP", "FLIP"),
                         multiplicity = c(1, 1)),
          # r34 : FASC : FADD3 : FLIP2[ ]c → FASC : FADD3 : FLIP[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r35 : FASC : FADD3 : CASP82[ CASP8 ]c → FASC : FADD3 : CASP83[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP82", "CASP82"),
                         multiplicity = c(1, 1)),
          # r36 : FASC : FADD3 : CASP83[ ]c → FASC : FADD3 : CASP82[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP83", "c"),
                         multiplicity = c(1, 1)),
          # r37 : FASC : FADD3 : CASP82[ FLIP ]c → FASC : FADD3 : CASP82 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP82", "FLIP"),
                         multiplicity = c(1, 1)),
          # r38 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : CASP82[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r39 : FASC : FADD3 : CASP8 : FLIP[ CASP8 ]c → FASC : FADD3 : CASP82 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP8:FLIP", "CASP8"),
                         multiplicity = c(1, 1)),
          # r40 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : CASP8 : FLIP[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r41 : FASC : FADD3 : CASP8 : FLIP[ FLIP ]c → FASC : FADD3 : CASP8 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:CASP82:FLIP", "FLIP"),
                         multiplicity = c(1, 1)),
          # r42 : FASC : FADD3 : CASP8 : FLIP2[ ]c → FASC : FADD3 : CASP8 : FLIP[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r43 : FASC : FADD3 : FLIP2[ CASP8 ]c → FASC : FADD3 : CASP8 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP2", "CASP8"),
                         multiplicity = c(1, 1)),
          # r44 : FASC : FADD3 : CASP8 : FLIP2[ ]c → FASC : FADD3 : FLIP2[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r45 : FASC : FADD3 : FLIP2[ FLIP ]c → FASC : FADD3 : FLIP3[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD3:FLIP2", "FLIP"),
                         multiplicity = c(1, 1)),
          # r46 : FASC : FADD3 : FLIP3[ ]c → FASC : FADD3 : FLIP2[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:FLIP3", "c"),
                         multiplicity = c(1, 1)),
          # r47 : FASC : FADD2[ CASP8 ]c → FASC : FADD2 : CASP8[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2", "CASP8"),
                         multiplicity = c(1, 1)),
          # r48 : FASC : FADD2 : CASP8[ ]c → FASC : FADD2[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r49 : FASC : FADD2[ FLIP ]c → FASC : FADD2 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2", "FLIP"),
                         multiplicity = c(1, 1)),
          # r50 : FASC : FADD2 : FLIP[ ]c → FASC : FADD2[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r51 : FASC : FADD2 : CASP8[ CASP8 ]c → FASC : FADD2 : CASP82[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8", "CASP8"),
                         multiplicity = c(1, 1)),
          # r52 : FASC : FADD2 : CASP82[ ]c → FASC : FADD2 : CASP8[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP82", "c"),
                         multiplicity = c(1, 1)),
          # r53 : FASC : FADD2 : CASP8[ FLIP ]c → FASC : FADD2 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:CASP8", "FLIP"),
                         multiplicity = c(1, 1)),
          # r54 : FASC : FADD2 : CASP8 : FLIP[ ]c → FASC : FADD2 : CASP8[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r55 : FASC : FADD2 : FLIP[ CASP8 ]c → FASC : FADD2 : CASP8 : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP", "CASP8"),
                         multiplicity = c(1, 1)),
          # r56 : FASC : FADD2 : CASP8 : FLIP[ ]c → FASC : FADD2 : FLIP[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r57 : FASC : FADD2 : FLIP[ FLIP ]c → FASC : FADD2 : FLIP2[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD2:FLIP", "FLIP"),
                         multiplicity = c(1, 1)),
          # r58 : FASC : FADD2 : FLIP2[ ]c → FASC : FADD2 : FLIP[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r59 : FASC : FADD[ CASP8 ]c → FASC : FADD : CASP8[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD", "CASP8"),
                         multiplicity = c(1, 1)),
          # r60 : FASC : FADD : CASP8[ ]c → FASC : FADD[ CASP8 ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r61 : FASC : FADD[ FLIP ]c → FASC : FADD : FLIP[ ]c k3f
          tibble::tibble(where = c("@here", "c"),
                         object = c("FASC:FADD", "FLIP"),
                         multiplicity = c(1, 1)),
          # r62 : FASC : FADD : FLIP[ ]c → FASC : FADD[ FLIP ]c k3r
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r63 : FASC : FADD2 : CASP82[ ]c → FASC : FADD2[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD2:CASP8", "c"),
                         multiplicity = c(1, 1)),
          # r64 : FASC : FADD3 : CASP83[ ]c → FASC : FADD3 : CASP8[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP83", "c"),
                         multiplicity = c(1, 1)),
          # r65 : FASC : FADD3 : CASP82 : FLIP[ ]c → FASC : FADD3 : FLIP[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82:FLIP", "c"),
                         multiplicity = c(1, 1)),
          # r66 : FASC : FADD3 : CASP82[ ]c → FASC : FADD3[ CASP8P41 2 ]c k4
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP82", "c"),
                         multiplicity = c(1, 1)),
          ## EVOLUTION
          # r67 : [ CASP8P41 2 ]c → [ CASP8∗2 ]c k5
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("FASC:FADD3:CASP8:FLIP2", "c"),
                         multiplicity = c(1, 1)),
          # r68 : [ CASP8∗2 , CASP3 ]c → [ CASP8∗2 : CASP3 ]c k6f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "CASP3"),
                         multiplicity = c(1, 1)),
          # r69 : [ CASP8∗2 : CASP3 ]c → [ CASP8∗2 , CASP3 ]c k6r
          tibble::tibble(where = c("@here"),
                         object = c("CASP8*2:CASP3"),
                         multiplicity = c(1)),
          # r70 : [ CASP8∗2 , CASP3∗ ]c → [ CASP8∗2 : CASP3 ]c k7
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "CASP3"),
                         multiplicity = c(1, 1)),
          # r71 : [ CASP8∗2, Bid ]c → [ CASP8∗2: Bid ]c k8f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "Bid"),
                         multiplicity = c(1, 1)),
          # r72 : [ CASP8∗2: Bid ]c → [ CASP8∗2, Bid ]c k8r
          tibble::tibble(where = c("@here"),
                         object = c("CASP8*2:Bid"),
                         multiplicity = c(1)),
          # r73 : [ CASP8∗2, tBid ]c → [ CASP8∗2: Bid ]c k7
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP8*2", "tBid"),
                         multiplicity = c(1, 1)),
          # r74 : [ tBid, Bax ]c → [ tBid : Bax ]c k9f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("tBid", "Bax"),
                         multiplicity = c(1, 1)),
          # r75 : [ tBid : Bax ]c → [ tBid, Bax ]c k9r
          tibble::tibble(where = c("@here"),
                         object = c("tBid:Bax"),
                         multiplicity = c(1)),
          # r76 : [ tBid : Bax, Bax ]c → [ tBid : Bax2 ]c k9f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("tBid:Bax", "Bax"),
                         multiplicity = c(1, 1)),
          # r77 : [ tBid : Bax2 ]c → [ tBid : Bax, Bax ]c k9r
          tibble::tibble(where = c("@here"),
                         object = c("tBid:Bax2"),
                         multiplicity = c(1)),
          ## m
          # r78 : tBid : Bax2[ Smac ]m → Smac∗[ ]m k10
          tibble::tibble(where = c("@here", "m"),
                         object = c("tBid:Bax2", "Smac"),
                         multiplicity = c(1, 1)),
          # r79 : tBid : Bax2[ Cyto.c ]m → Cyto.c∗[ ]m k10
          tibble::tibble(where = c("@here", "m"),
                         object = c("tBid:Bax2", "Cyto.c"),
                         multiplicity = c(1, 1)),
          ## STANDARD
          # r80 : [ Smac∗, XIAP ]c → [ Smac∗ : XIAP ]c k11f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Smac*", "XIAP"),
                         multiplicity = c(1, 1)),
          # r81 : [ Smac∗ : XIAP ]c → [ Smac∗, XIAP ]c k11r
          tibble::tibble(where = c("@here"),
                         object = c("Smac:XIAP"),
                         multiplicity = c(1)),
          # r82 : [ Cyto.c∗, Apaf ]c → [ Cyto.c∗ : Apaf : ATP ]c k12f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Cyto.c*", "Apaf"),
                         multiplicity = c(1, 1)),
          # r83 : [ Cyto.c∗ : Apaf : ATP ]c → [ Cyto.c∗, Apaf ]c k12r
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP"),
                         multiplicity = c(1)),
          # r84 : [ Cyto.c∗ : Apaf : ATP, CASP9 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9 ]c k13f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Cyto.c*:Apaf:ATP", "CASP9"),
                         multiplicity = c(1, 1)),
          # r85 : [ Cyto.c∗ : Apaf : ATP : CASP9 ]c → [ Cyto.c∗ : Apaf : ATP, CASP9 ]c k13r
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP9"),
                         multiplicity = c(1)),
          # r86 : [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9 ]c → [ Cyto.c∗ : Apaf : ATP : CASP92 ]c k14f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP9", "CASP9"),
                         multiplicity = c(1, 1)),
          # r87 : [ Cyto.c∗ : Apaf : ATP : CASP92 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9 ]c k14r
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP92"),
                         multiplicity = c(1)),
          # r88 : [ Cyto.c∗ : Apaf : ATP : CASP92 ]c → [ Cyto.c∗ : Apaf : ATP : CASP9, CASP9∗ ]c k15
          tibble::tibble(where = c("@here"),
                         object = c("Cyto.c*:Apaf:ATP:CASP92"),
                         multiplicity = c(1)),
          # r89 : [ CASP9∗, CASP3 ]c → [ CASP9∗ : CASP3 ]c k16f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP9*", "CASP3"),
                         multiplicity = c(1, 1)),
          # r90 : [ CASP9∗ : CASP3 ]c → [ CASP9∗, CASP3 ]c k16r
          tibble::tibble(where = c("@here"),
                         object = c("CASP9*:CASP3"),
                         multiplicity = c(1)),
          # r91 : [ CASP9∗ : CASP3 ]c → [ CASP9∗, CASP3∗ ]c k17
          tibble::tibble(where = c("@here"),
                         object = c("CASP9*:CASP3"),
                         multiplicity = c(1)),
          # r92 : [ CASP9, XIAP ]c → [ CASP9 : XIAP ]c k18f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP9", "XIAP"),
                         multiplicity = c(1, 1)),
          # r93 : [ CASP9 : XIAP ]c → [ CASP9, XIAP ]c k18r
          tibble::tibble(where = c("@here"),
                         object = c("CASP9:XIAP"),
                         multiplicity = c(1)),
          # r94 : [ CASP3∗, XIAP ]c → [ CASP3∗ : XIAP ]c k19f
          tibble::tibble(where = c("@here", "@here"),
                         object = c("CASP3*", "XIAP"),
                         multiplicity = c(1, 1)),
          # r95 : [ CASP3∗ : XIAP ]c → [ CASP3∗, XIAP ]c k19r
          tibble::tibble(where = c("@here"),
                         object = c("CASP3*:XIAP"),
                         multiplicity = c(1)),
          ## m
          # r96 : Bax[ Bcl2 ]m → [ Bcl2 : Bax ]m k20f
          tibble::tibble(where = c("@here", "m"),
                         object = c("Bax", "Bcl2"),
                         multiplicity = c(1, 1)),
          # r97 : [ Bcl2 : Bax ]m → Bax[ Bcl2 ]m k20r
          tibble::tibble(where = c("@here"),
                         object = c("Bcl2:Bax"),
                         multiplicity = c(1)),
          # r960 : Bid[ Bcl2 ]m → [ Bcl2 : Bid ]m k20f
          tibble::tibble(where = c("@here", "m"),
                         object = c("Bid", "Bcl2"),
                         multiplicity = c(1, 1)),
          # r970 : [ Bcl2 : Bid ]m → Bid[ Bcl2 ]m k20r
          tibble::tibble(where = c("@here"),
                         object = c("Bcl2:Bid"),
                         multiplicity = c(1)),
          # r9600 : tBid[ Bcl2 ]m → [ Bcl2 : tBid ]m k20f
          tibble::tibble(where = c("@here", "m"),
                         object = c("tBid", "Bcl2"),
                         multiplicity = c(1, 1)),
          # r9700 : [ Bcl2 : tBid ]m → tBid[ Bcl2 ]m k20r
          tibble::tibble(where = c("@here"),
                         object = c("Bcl2:tBid"),
                         multiplicity = c(1))





        ),
        propensity = seq(1, 1/n_rules, -1/n_rules)
      ),

      "Properties" = tibble::tibble(
        System = NA,
        PLingua_model = NA,
        N_membranes = NA,
        N_objects = 53,
        N_rules = 99,
        Max_depth_in_rules = NA # For now at least
      )
    )
    return(fas_rap)
  }
  ##################################################
  ################# END OF #########################
  ################# FAS ############################
  ##################################################




  ##################################################
  ################# BEGINNING OF ###################
  ################# QUORUM #########################
  ##################################################
  if (dataset == "QUORUM") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }
  ##################################################
  ################# END OF #########################
  ################# QUORUM #########################
  ##################################################





  ##################################################
  ################# BEGINNING OF ###################
  ################# TEMPLATE #######################
  ##################################################
  if (dataset == "TEMPLATE") {
    n_rules = 4
    template_exit = list(

      "Configuration" = tibble::tibble(
        environment = c(0, 0, 0),
        id = c(0, 1, 2),
        label = c(0, 1, 2),
        objects = list(
          tibble::tibble(object = "@filler", multiplicity = 1),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2)
        ),
        superM = c(NA, 0, 1), # Given by ID # END: We could have more than one parent

        subM = list(
          tibble::tibble(children = 1),
          tibble::tibble(children = 2),
          tibble::tibble(children = NA)),
        charge = c(0, 1, -1),
        other_params = c(NA, NA, NA)
      ),

      "Rules" = tibble::tibble(
        rule_id = 1:n_rules,
        dissolves = c(rep(FALSE, n_rules)),
        priority = rep("-", n_rules),

        main_membrane_label = rep(1, n_rules),

        lhs = list(
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("a", 2),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = "@here",
                         object = "a",
                         multiplicity = 1),
          tibble::tibble(where = c("@here", 2),
                         object = c("a", "a"),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = c("@here", "@here"),
                         object = c("a", "b"),
                         multiplicity = c(1, 1))
        ),

        rhs = list(
          tibble::tibble(where = c("@here", "@here"),
                         object = c("c", "d"),
                         multiplicity = 3:4),
          tibble::tibble(where = 2,
                         object = "b",
                         multiplicity = 1),
          tibble::tibble(where = c("@here", 2),
                         object = c("b", "c"),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = 2,
                         object = "c",
                         multiplicity = 3)
        ),
        propensity = seq(1, 1/n_rules, -1/n_rules)
      ),

      "Properties" = tibble::tibble(
        System = NA,
        PLingua_model = NA,
        N_membranes = NA,
        N_rules = NA,
        Max_depth_in_rules = NA # For now at least
      )
    )
    return(template_exit)
  }
  ##################################################
  ################# END OF #########################
  ################# TEMPLATE #######################
  ##################################################
}
