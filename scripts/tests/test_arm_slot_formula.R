e <- new.env(parent=globalenv())
find_functions <- function(x) {
 if(!is.call(x)&&!is.expression(x)) return()
 if(is.call(x)&&identical(x[[1]],as.name("<-"))&&is.symbol(x[[2]])&&as.character(x[[2]]) %in% c("arm_angle_deg_from_rel","movement_line_slope")) eval(x,e)
 for(y in as.list(x)) tryCatch({if(is.call(y)||is.expression(y)) find_functions(y)},error=function(e) NULL)
}
find_functions(parse("WallyApps/PitchingApp/PitchingApp.R"))
stopifnot(abs(e$arm_angle_deg_from_rel(6,2,74/12)-atan2(8/12,2)*180/pi)<1e-10,
 abs(e$arm_angle_deg_from_rel(64/12,-2,74/12))<1e-10,
 e$arm_angle_deg_from_rel(4,-2,74/12)<0,
 e$arm_angle_deg_from_rel(6,0,74/12)==90)
cat("Horizontal, sidearm, submarine and vertical arm-slot checks passed.\n")
