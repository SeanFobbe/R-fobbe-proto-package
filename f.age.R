#'## f.age: Calculate human age
#' Calculates age from date of birth to target date. Vectorized.

#' @param birthdate Date of birth
#' @param target Target date

f.age <- function(birthdate, target){
    if (is.na(birthdate) | is.na(target)){
        return(NA)
    }else{
        age <- year(target) - year(birthdate) - 1
        if (month(target) > month(birthdate)){
            age <- age + 1
        }
        if (month(target) == month(birthdate) && mday(target) >= mday(birthdate)){
            age <- age + 1
        }
        return(age)
    }
}

f.age <- Vectorize(f.age)
