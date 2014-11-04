/^--/{
  s/^--\( |\)* *// 
  h
  :loop
  n
  /^[-]/{
    s/^-- *//
    H
    b loop
  }
  /^ *$/{
    b skip
  }
  /^data/{
    b skip
  }
  s/^\([^ ]\{1,\}\) ::.*/---\
title: \1\
category: code_import\
---\
`&`\
/
  p
  x
  p
  :skip
}