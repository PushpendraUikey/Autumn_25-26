#include <iostream>
#include <functional>
#include <optional>  // C++17

// We'll use std::optional<T> to represent Maybe<T>

template<typename A, typename F>
auto bind(std::optional<A> ma, F f) -> decltype(f(std::declval<A>()))
{
    if (ma.has_value())
        return f(*ma);   // apply f to the unwrapped value
    else
        return {};       // return empty optional (Nothing)
}

std::optional<int> safe_divide(int x, int y) {
    if (y == 0) return {};
    return x / y;
}

// General Monad Intuition in C++
template<typename T>
struct Monad {
    T value;
    // return  :: a -> m a
    static Monad<T> pure(T v);
    // >>=     :: m a -> (a -> m b) -> m b
    template<typename F>
    auto bind(F f) -> Monad<decltype(f(value).value)>;
};


int main() {
    auto result =
        bind(safe_divide(10, 2), [](int r1) {
        return bind(safe_divide(r1, 5), [](int r2) {
        return std::optional<int>{r2 + 3};
        });
    });

    if (result.has_value())
        std::cout << "Result = " << *result << "\n";
    else
        std::cout << "Computation failed\n";
}
