

# macro defining the variable 'target'
target = poisson
# below a macro defining the list of object files in the project;
# the objects are recompiled in the order in which they appear in the objs list
# newline is "\" and must be the last character on the line
# Tabs are not required/used in the definition of macros
objs = poisson_problem.o\
modglobal.o

# linking:the target depends on the objects
$(target):$(objs)
	f90 -free $(objs) -o $(target)

#dependencies:
poisson_problem.o : poisson_problem.f modglobal.o 
	f90 -free -c poisson_problem.f

modglobal.o : modglobal.f
	f90 -free -c modglobal.f

new: clean $(target)

clean:
	rm -fr $(objs)
