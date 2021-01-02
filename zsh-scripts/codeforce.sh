function codeforceinit {
  prefix="codeforce"
  echo "\033[35mMaking $PWD/$prefix-$1 \033[0m"
  rm *.current-position
  touch "$1.current-position"
  mkdir "$PWD/$prefix-$1"
  cd "$PWD/$prefix-$1"

  echo "\033[35mMaking $1.cpp and filling content \033[0m"
  touch "$1.cpp"
  echo "#include <bits/stdc++.h>\nusing namespace std;\n\nint main() {\n  cout << \"hi\";\n  return 0;\n}" > "$1.cpp"
  vim "$1.cpp"
}
