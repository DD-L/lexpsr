emcc ./regex_builder.cpp -o html/js/regex_builder.js -s MODULARIZE=1 -s EXPORT_NAME='MyModule' -std=c++17 -Wall -O3 -DNDEBUG --bind
