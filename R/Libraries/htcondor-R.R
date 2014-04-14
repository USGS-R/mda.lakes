
condor.write.submit = function(fPath, executable, input.files="", arguments="" ){
  
  fid = file(fPath,'wb')
  
  writeLines(sprintf("executable=%s", executable), fid)
  writeLines(sprintf("arguments=%s", arguments), fid)
  
  writeChar("transfer_input_files=", fid, eos=NULL)
  
  if(length(input.files) == 1){
    writeLines(input.files, fid)
    
  }else if(length(input.files > 1)){
    writeChar(input.files[1], fid, eos=NULL)
    
    for(i in 2:length(input.files)){
      writeChar(',', fid, eos=NULL)
      writeChar(input.files[i], fid, eos=NULL)
    }
    
    writeLines('',fid)
  }
  
  writeLines("universe   = vanilla", fid)
  writeLines("output = phase1.out", fid)
  writeLines("error = phase1.err", fid)
  writeLines("log = phase1.log", fid)
  writeLines("requirements = (TARGET.Arch == \"X86_64\") && ((TARGET.OpSys == \"WINDOWS\")) && (Machine =!= \"hanson-i5.ad.wisc.edu\") && (Machine =!= \"Simulator.ad.wisc.edu\")&& (Machine =!= \"i7top\")", fid)

  writeLines("environment=\"PATH='C:\\Program Files\\R\\R-2.15.2\\bin\\i386;C:\\Program Files\\R\\R-2.15.3\\bin\\i386;C:\\Windows\\system32'\"", fid)
  writeLines("should_transfer_files = YES", fid)
  writeLines("+WantFlocking = false", fid)
  writeLines("when_to_transfer_output = ON_EXIT", fid)
  writeLines("notification = never", fid)
  writeLines("queue", fid)
  
  close(fid)
}