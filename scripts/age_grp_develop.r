# age_grp_develop.r
# 

ages_int <- 0:102
age_grps  <- age.cat(.ages = ages_int, .by = age.groups$Annual)

age.group.encode(groups = as.character(ages_int))
tmp <- age.group.encode(groups = age_grps)
age.group.decode(groups = tmp)

# alternate encoding
# for 5-14

intToBits(1)
uuu <- bitwShiftL(a = 1, n = 8)
uuu
intToBits(uuu) 
bitwShiftR(uuu, 8)

all <- bitwOr(a = uuu, b = 2)
intToBits(all)
uuu_mask <- bitwShiftL(1111111, 9)
bitwAnd(all, uuu_mask) |> bitwShiftR(n = 8)

                      