* On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

As I understand it this cannot be fixed locally but is because something is missing on the 
Fedora test platform. 

* On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
  
Not sure what creates this file, or why it is not removed on Windows. Should not be a problem for CRAN build. 
