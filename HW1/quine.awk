# Quine in AWK, based on the C example from https://en.wikipedia.org/wiki/Quine_(computing)#Examples
BEGIN{prog="BEGIN{s=%c%s%c;printf(s,34,s,34);}";printf(s,34,s,34);}

# And this one prints itself three times
BEGIN{prog="BEGIN{s=%c%s%c;for(i=1;i<=3;++i)printf(s,34,s,34);}";for(i=1;i<=3;++i)printf(s,34,s,34);}
