
change_pos <- function(x) glue("+{x}")
change_neg <- function(x) "-x"
no_change <- function(x) "x"
CFBWeek <- function() {
  date <- Sys.time()
  month <- as.numeric(date %>% stri_sub(6,7))
  mday <- as.numeric(date %>% stri_sub(9,10))
  hour <- as.numeric(date %>% stri_sub(12,13))
  if(month<9){0} else{
    if(month==9){
      if(mday<5){1} else{
        if(mday==5){
          if(hour>3){2}else{1}}else{
            if(mday<12){2} else{
              if(mday==12){
                if(hour>3){3}else{2}}else{
                  if(mday<19){3} else{
                    if(mday==19){
                      if(hour>3){4}else{3}}else{
                        if(mday<26){4} else{
                          if(mday==26){
                            if(hour>3){5}else{4}}else{NA}}}}}}}}}else{
                              if(month==10){
                                if(mday<3){5} else{
                                  if(mday==3){
                                    if(hour>3){6}else{5}}else{
                                      if(mday<10){6} else{
                                        if(mday==10){
                                          if(hour>3){7}else{6}}else{
                                            if(mday<17){7} else{
                                              if(mday==17){
                                                if(hour>3){8}else{7}}else{
                                                  if(mday<24){8} else{
                                                    if(mday==24){
                                                      if(hour>3){9}else{8}}else{
                                                        if(mday<31){9}else{
                                                          if(mday==31){
                                                            if(hour>3){10}else{9}}else{NA}}}}}}}}}}}else{
                                                              if(month==11){
                                                                if(mday<7){10}else{
                                                                  if(mday==7){
                                                                    if(hour>3){11}else{10}}else{
                                                                      if(mday<14){11}else{
                                                                        if(mday==14){
                                                                          if(hour>3){12}else{11}}else{
                                                                            if(mday<21){12}else{
                                                                              if(mday==21){
                                                                                if(hour>3){13}else{12}}else{
                                                                                  if(mday<28){13}else{
                                                                                    if(mday==28){
                                                                                      if(hour>3){14}else{13}}else{NA}}}}}}}}}else{
                                                                                        if(month==12){
                                                                                          if(mday<5){14}else{
                                                                                            if(mday==5){
                                                                                              if(hour>3){15}else{14}}else{
                                                                                                if(mday<12){15}else{
                                                                                                  if(mday<19){16}else{
                                                                                                    if(mday==19){
                                                                                                      if(hour>3){17}else{17}}else{17}}}}}}}}}}}
