# Paths must be altered to match your location for libgiq.so
# All of the DAT files must be in the same location as libgiq.so 
# Using rpath is probably not the best way to install a sharable object file on Linux, consider a standard binary location on the path.
gcc -L/home/paul/Projects/GridInQuestII/Examples/C -Wl,-rpath=/home/paul/Projects/GridInQuestII/Examples/C -Wall -o test test.c -lgiq