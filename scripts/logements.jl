################################################################################
## Logements ordinaires
################################################################################

# Load pakcages
using CSV, DataFrames

# Load data
df = CSV.read("./data/FD_LOGEMT_2020.csv", DataFrame;
    select=["COMMUNE"], types=String, delim=";"
)

# Groupby COMMUNE
df_commune = groupby(df, :COMMUNE);
df_commune = combine(df_commune, nrow => :N_COMMUNE)

df_region = transform(
    df_commune, :COMMUNE => ByRow(x -> SubString(x, 1, 2)) => :REGION
)
df_region = groupby(df_region, :REGION);
df_region = combine(df_region, )