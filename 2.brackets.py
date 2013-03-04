PAIRS  = {'{':'}', '[':']', '(':')'}
STRING = """{{[{{{{}}{{}}}[]}[][{}][({[((\
{{[][()()]}}{[{{{}}}]}))][()]\
{[[{((()))({}(())[][])}][]()]\
}{()[()]}]})][]]}{{}[]}}"""

stack = []
for (i, c) in enumerate(STRING):
  if c in PAIRS.keys():
    stack.append(c)
  elif stack and c == PAIRS[stack[-1]]:
    stack.pop()
  else:
    print i