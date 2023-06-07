# Memory Safety Project
This project was a complete code audit for a program that provides an image viewer for a fake company named "Badly Coded Inc".  The program allows the user to view images in a GUI or convert them to a different format.

The sample images are provided in the folder [sample_images](sample-images/)
The file [bcimgview](bcimgview.c) is the original program.
The file [bcimgview_fixed](bcimgview_fixed.c) is the program with 3 fixes that ensure meory safety.

This project took about half of the semester.  The first part was code audtiing and threat modeling the program in a 3-4 page report.
The second part involved attacking the bugs to take control of the program flow and perform fake "attack" functions.
For the thrid part we were tasked with fixing the vulnerabilities and finalzing that included the original threat modell, results from code auduting, and describing the vulnerabilites and how we attacked and fixed them.  

Most of the debugging was done in GDB.
