f'(x)

f'''(x, y, z)

P.show(x)

(\pA0 -> apply'to_all_in'(f, pA0))

apply'to_all_in'(f, l)

(\pA0 -> apply'to_all_in'((\pA0 -> P.Just(pA0)), pA0))

(\pA0 -> apply'to_all_in'((\x' -> p1st(x')), pA0))

(\pA0 -> apply'to_all_in'((\pA0 -> a'to_str(pA0)), pA0))

(\pA0 -> f'''(pA0, y, z))

(\(pA0, pA1) -> f'''(x, pA0, pA1))

(\(pA0, pA1) -> f'''(pA0, y, pA1))

(\(pA0, pA1) -> f'''(pA0, pA1, z))

f'(x)

(\pA0 -> f''(pA0, y))

(\(pA0, pA1) -> f'''(pA0, pA1, z))

