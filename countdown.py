def countdown_loop(n):
    """
    Prints numbers from n up to (but not including) 100000.
    This version uses a while loop.
    """
    current = n
    while current < 100000:
        print(current)
        current += 1
    # The Scheme version returns 0 when the condition is met.
    # In Python, a function returns None by default if no return statement is hit.
    # We can explicitly return 0 to match.
    return 0


countdown_loop(0)
