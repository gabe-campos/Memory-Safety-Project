# Memory Safety Project
This project was a complete code audit for a program made for a fake company named "Badly Coded Inc".  The program allows the user to view image files in a GUI or convert them to a different format.  The image file formats are specific to the "Badly Coded" company, and can also only be converted between these formats.

The sample images are provided in the folder [sample_images](sample-images/)
The file [bcimgview](bcimgview.c) is the original program.
The file [bcimgview_fixed](bcimgview_fixed.c) is the program with 3 fixes that ensure meory safety.

Either file can be compiled with the commmand:
```
gcc -no-pie -fno-stack-protector -Og -g -Wall \
$(pkg-config --cflags gtk+-3.0) \
<bcimgview.c/bcimgview_fixed.c> -o <bcimgview/bcimgview_fixed> \
-lgtk-3 -lgobject-2.0 -lglib-2.0 -lgdk_pixbuf-2.0 -lm
```

This project took about half of the semester.  The first part was code audtiing and threat modeling the program in a 3-4 page report.
The second part involved attacking the vulnerabilites to take control of the program flow and execute fake "attack" functions.
For the thrid part we were tasked with fixing the vulnerabilities and finalzing a report including the original threat model, results from code auduting, and describing the vulnerabilites and how we attacked and fixed them.  

Most of the debugging was done in GDB.
