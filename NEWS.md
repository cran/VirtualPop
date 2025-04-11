# VirtualPop 1.0.2 Published June 23 2022

# VirtualPop 2.0.2 Published 2024

New functions

-   GetRatesC(): reads cohort mortality rates from the HMD and cohort
    conditional fertility rates from the HFD. They are used to build
    virtual populations from cohort data.
-   BuildViP(): builds a virtual population in a single step.
-   PartnerSearch(): replaces Partnership(). Individuals search for
    partner in the same generation.
-   Other functions are updated to make the simulation more efficient.

Data

-   New: ratesC.rda : cohort rates of mortality and fertility. The
    cohort is the USA 1964 birth cohort. Since not all cohort members
    are deceased in 2021, the mortality data are augmented by period
    data for 2021 (see Tutorial).
-   Removed: dpopus.rda. These data are used in the validation of the
    simulation. They are moved to Zenodo.
-   Replaced: dLH:
    -   dLH is replaced by fertility histories based on 2021 rates.
    -   The IDs of children and the ages of mother at time of birth are
        omitted from dLH. They are retrieved from the data on children
        (next generation).
    -   New variable: udated: date of union formation (not used).
    -   Attributes added to distinguish between period and cohort data.

Vignettes

-   The vignette “Validation of a virtual population” is not included in
    the vignette folder, but the PDF version is included in the doc
    folder because the comparison of virtual and real populations uses
    register and survey data that cannot be distributed.
-   The other vignettes have been updated.
