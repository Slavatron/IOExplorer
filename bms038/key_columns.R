# DEFINE OPTIONS FOR DROP-DOWN MENUS for Pre-therapy Tab
Exome <- list("log10mut" = "Mutation Load (log)", 
               "log10na" = "Neopeptide Load (log)",
              "log10na2" = "Neoantigen Load (log)",
               "thresh95.muts" = "Clonal Mutation Load",   
               "Signature.1" = "Aging Signature",
               "Signature.7" ="UV Signature",
              "Davoli_SCNA" = "Davoli SCNA",
              "facets_CNA_Genome" = "Fraction Genome CNA"
              )

# Need to add in other T-cell metrics from Jonathan here
TCR <- list("TCR.D90" = "D90",
            "TCR_TIL" = "Tumor Infiltrating Lymphocyte Fraction",
            "productive_templates" = "Productive Template Count",
            "productive_rearrangements" = "Productive Rearrangement Count",
            "TCR.Num_CDR3" = "CDR3 Count",
            "productive_entropy" = "Shannon Entropy of Productive CDR3s",
            "productive_clonality" = "Clonality of Productive CDR3s"
            
#            "TCR.DeltaH" = "TCR.DeltaH",
#            "TCR.DeltaCL" = "TCR.DeltaCL"
            )

# Need to add other signatures from Alexis here
RNASeq <- list("cytscore" = "Cytolytic Score",
#               "B.cells.naive" = "Naïve B-Cells (RNA)",
#               "B.cells.memory" = "Memory B-Cells (RNA)",
#               "Plasma.cells" = "Plasma Cells (RNA)",
#               "T.cells.CD8" = "CD8 T-Cells (RNA)",
#               "T.cells.CD4.naive" = "Naïve CD4 T-Cells (RNA)",
#               "T.cells.CD4.memory.resting" = "Resting Memory CD4 T-Cells (RNA)",
#               "T.cells.CD4.memory.activated" = "Activated Memory CD4 T-Cells (RNA)",
#               "T.cells.follicular.helper" = "Follicular Helper T-Cells (RNA)",
#               "T.cells.regulatory..Tregs." = "Regulatory T-Cells (RNA)",
#               "T.cells.gamma.delta" = "Gamma Delta T-Cells (RNA)",
#               "NK.cells.resting" = "Resting Natural Killer Cells (RNA)",
#               "NK.cells.activated" = "Activated Natural Killer Cells (RNA)",
#               "Monocytes" = "Monocytes (RNA)",
#               "Macrophages.M0" = "M0 Macrophages (RNA)",
#               "Macrophages.M1" = "M1 Macrophages (RNA)",
#               "Macrophages.M2" = "M2 Macrophages (RNA)",
#               "Dendritic.cells.resting" = "Resting Dendritic Cells (RNA)",
#               "Dendritic.cells.activated" = "Activated Dendritic Cells (RNA)",
#               "Mast.cells.resting" = "Resting Mast Cells (RNA)",
#               "Mast.cells.activated" = "Activated Mast Cells (RNA)",
#               "Eosinophils" = "Eosinophils (RNA)",
#               "Neutrophils" = "Neutrophils (RNA)",
               "CIB.Tcell" = "T-Cells (Cibersort)",
               "CIB.Bcell" = "B-Cells (Cibersort)",
               "CIB.NK" = "Natural Killer Cells (Cibersort)",
               "CIB.Mac" = "Macrophages (Cibersort)",
               "CIB.Mast" = "Mast Cells (Cibersort)",
               "CIB.Misc" = "Miscellaneous (Cibersort)")

# Need to make sure these are exact columns that are used in paper
ImmuneDeconvolution  <- list("T.cells.CD8" = "CD8 T-Cells",
#                             "T.cells.CD4.naive" = "CD4 T-Cells",
                             "T.cells.CD4.memory.resting" = "Resting CD4 T-Cells",
                             "T.cells.CD4.memory.activated" = "Activated CD4 T-Cells",
                             "NK.cells.activated" = "NK Cells",
                             "Macrophages.M1" = "Macrophages",
                             "EST.StromalScore" = "Stromal Score (EST)",
                             "EST.ImmuneScore"	= "Immune Infiltration (EST)",
                             "EST.ESTIMATEScore" = "ESTIMATE Score (EST)",
                             "EST.TumorPurity" = "Tumor Purity (EST)"
                             )

IHC <- list("PDL1.1plus.pos" = "PDL1 Positive",
            "Perc.CD8.of.Total" = "Percent CD8",
            "Perc.PD1.of.Total" = "Percent PD1"
                            )


# COMBINE INTO SINGLE LISTS
pre_choice1 = list("Exome" = unlist(Exome), "RNASeq" = unlist(RNASeq), "IHC" = unlist(IHC), "TCR" = unlist(TCR), "Immune Deconvolution" = unlist(ImmuneDeconvolution))

# Clinical features
Clin = list("Sex" = "Sex",
            "Age" = "Age",
            "Cohort" = "Cohort",
#            "PreTreat_Exome" = "Pre-Treatment Exome Sequenced",
#            "OnTreat_Exome" = "On-Treatment Exome Sequenced",
#            "TCR_Data" = "TCR Sequenced",
#            "SubtypeEZ" = "Mutational Subtype",
            "Stage" = "Stage"
            )

pre_choice2 = list("Clin" = unlist(Clin), "Exome" = unlist(Exome), "RNASeq" = unlist(RNASeq), "IHC" = unlist(IHC), "TCR" = unlist(TCR), "Immune Deconvolution" = unlist(ImmuneDeconvolution))
# Copy Number Features
#CNV = list("")
###################################################

# DEFINE OPTIONS FOR DROP-DOWN MENUS for Diff-therapy Tab
Exome_Diff <- list("delt.log10mut" = "Change in Mutation Load (log)", 
              "delt.log10na" = "Change in Neopeptide Load (log)", 
              "delt.log10na2" = "Change in Neoantigen Load (log)", 
              "delt.thresh95.muts" = "Change in Clonal Mutation Load",
              "Clonal_Contraction" = "Clonal Contraction vs. Persistance",
              "delt.Signature.1" = "Change in Aging Signature",
              "delt.Signature.7" ="Change in UV Signature"
)

# Need to add in other T-cell metrics from Jonathan here
TCR_Diff <- list("delt.TCR.D90" = "Change in D90",
            "delt.TCR.Num_CDR3" = "Change in Num CDR3",
            "delt.TCR.fraction_TILs" = "Change in Fraction of TILs"
)

# Need to add other signatures from Alexis here
RNASeq_Diff <- list("delt.cytscore" = "Change in Cytolytic Score",
                    "delt.CIB.Tcell" = "Change in T-Cells (Cibersort)",
                    "delt.CIB.Bcell" = "Change in B-Cells (Cibersort)",
                    "delt.CIB.NK" = "Change in Natural Killer Cells (Cibersort)",
                    "delt.CIB.Mac" = "Change in Macrophages (Cibersort)",
                    "delt.CIB.Mast" = "Change in Mast Cells (Cibersort)",
                    "delt.CIB.Misc" = "Change in Miscellaneous (Cibersort)"
                    )

# Need to make sure these are exact columns that are used in paper
ImmuneDeconvolution_Diff  <- list("delt.T.cells.CD8" = "CD8 T-Cells",
                             "delt.T.cells.CD4.naive" = "CD4 T-Cells",
                             "delt.NK.cells.activated" = "NK Cells",
                             "delt.Macrophages.M1" = "Macrophages"
)

IHC_Diff <- list("delt.PDL1.1plus.pos" = "Change in PDL1 Positive",
            "delt.Perc.CD8.of.Total" = "Change in Percent CD8",
            "delt.Perc.PD1.of.Total" = "Change in Percent PD1"
)


# COMBINE INTO SINGLE LIST
diff_choice1 = list("Exome" = unlist(Exome_Diff), "RNASeq" = unlist(RNASeq_Diff), "IHC" = unlist(IHC_Diff), "TCR" = unlist(TCR_Diff), "Immune Deconvolution" = unlist(ImmuneDeconvolution_Diff))

###################################################

#selector_choice1 = pre_choice1 
