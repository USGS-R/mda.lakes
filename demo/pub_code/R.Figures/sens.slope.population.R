all.slopes.perms = function(data){
  
  perm.i = combn(1:nrow(data),2)
  perm1 = data[perm.i[1,],]
  perm2 = data[perm.i[2,],]
    
  return((perm1$temp-perm2$temp)/(perm1$date - perm2$date))
}
