BEGIN {
  state = "start"
  i = 0
}

/---/ {
  if(state == "start") {
    state = "prelude"
  }
  else {
    if(state == "prelude") {
      state = "markdown"
    }
    else {
      close(filename)
      i++
      

      state = "prelude"
    }
  }
}

{
    filename = sprintf("haddock/md/function-%02d.md", i)
    if(state == "prelude") {
      print >> filename
    }
    else {
      if (state == "markdown") {
        print >> filename
      }
    }
}

END {
  print "Written " int(i) " function documentation files as markdown"
}