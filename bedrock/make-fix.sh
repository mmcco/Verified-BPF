# replaces $(MAKE) with ${MAKE}

# This allows bedrock to compile on systems such as *BSDs that
# don't use GNU make by default.

perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./doc/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./examples/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./examples/comparison/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./examples/x86/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./platform/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./platform/cito/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./src/Makefile
perl -pi -e 's/\(MAKE\)/{MAKE}/g' ./src/reification/Makefile
