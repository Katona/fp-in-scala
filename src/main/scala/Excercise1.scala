object Excercise1 {
    def isSorted[A](a: Array[A], gt: (A, A) => Boolean): Boolean = {
        if (a.length < 2) {
            true;
        }
        for (i <- 0 until a.length - 1) {
            if (!gt(a(i), a(i + 1))) {
                return false;
            }
        }
        true;

    }
}