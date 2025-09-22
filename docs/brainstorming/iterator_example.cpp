#include <vector>
#include <optional>
#include <stdlib.h>
#include <stdio.h>

using mapping = int(*)(int);
int next_counter = 0;

constexpr void copy(int* dst, int* src, size_t amount) {
    for (size_t i = 0; i < amount; i++) {
        dst[i] = src[i];
    }
}

constexpr static int* alloc(size_t size) noexcept {
    return new (std::nothrow) int[size];
}

constexpr static void dealloc(int* ptr) noexcept {
    delete[] ptr;
}

struct option {
    int value;
    bool has;

    constexpr static option some(int val) noexcept {
        return option {
            .value = val,
            .has = true
        };
    }

    constexpr static option none() noexcept {
        return option {
            .value = 0,
            .has = false
        };
    }

    constexpr operator bool() const noexcept {
        return has;
    }

    constexpr int operator *() const noexcept {
        return value;
    }
};

struct vec {
    int* data;
    size_t length, capacity;

    constexpr vec() noexcept :
        data(nullptr), length(0), capacity(0) {}
    constexpr vec(vec&& other) noexcept :
        data(other.data), length(other.length), capacity(other.capacity) {
            other.data = nullptr;
        }
    constexpr vec(size_t known_size) noexcept :
        data(alloc(known_size * 4)), length(0), capacity(known_size) {}

    constexpr ~vec() noexcept { dealloc(data); }

    __attribute__((always_inline))
    constexpr void push_back(int x) noexcept {
        if (length == capacity) {
            capacity = (capacity + 1) * 2;

            int* new_data = alloc(capacity);
            copy(new_data, data, length);
            dealloc(data);
            data = new_data;
        }

        data[length++] = x;
    }

    __attribute__((always_inline))
    constexpr int& operator[](size_t index) noexcept {
        return data[index];
    }
};

template <typename InnerFunctor>
struct map_t {
    InnerFunctor data;
    mapping func;

    constexpr map_t(InnerFunctor data, mapping func) noexcept
        : data(data), func(func) {}

    __attribute__((always_inline))
    constexpr option next() noexcept {
        if (auto val = data.next(); val) {
            return option::some(func(*val));
        } else {
            return option::none();
        }
    }

    __attribute__((always_inline))
    constexpr map_t<map_t> map(mapping mapping) noexcept {
        return map_t<map_t> { *this, mapping };
    }

    __attribute__((always_inline))
    constexpr std::optional<int> known_size() noexcept {
        return data.known_size();
    }

    __attribute__((always_inline))
    constexpr vec collect() noexcept {
        vec data = [this]() constexpr {
            if (auto size = known_size(); size) {
                return vec(*size);
            } else {
                return vec();
            }
        }();

        while (auto val = next()) {
            data.push_back(*val);
        }

        return data;
    }
};

struct iterator_t {
    int* ptr;
    int* start;
    int* end;

    option compile_time_size;

    constexpr iterator_t(int* start, int* end) noexcept
        : ptr(start), start(start), end(end), compile_time_size(end - start) {}

    constexpr iterator_t(const vec& data) noexcept
        : ptr(data.data), start(data.data), end(data.data + data.length), compile_time_size(data.length) {}

    __attribute__((always_inline))
    constexpr map_t<iterator_t> map(mapping mapping) noexcept {
        return map_t<iterator_t> { *this, mapping };
    }

    __attribute__((always_inline))
    constexpr option known_size() noexcept {
        return compile_time_size;
    }

    __attribute__((always_inline))
    constexpr option next() noexcept {
        // next_counter++;

        if (ptr == end)
            return option::none();

        return option::some(*ptr++);
    }
};

__attribute__((always_inline))
constexpr vec test() noexcept {
    vec vals;
    vals.push_back(1);
    vals.push_back(2);
    vals.push_back(3);

    return iterator_t { vals }
        .map([](int i) constexpr { return i * 2; })
        .map([](int i) constexpr { return i * 4; })
        .collect();
}

__attribute__((always_inline))
constexpr vec test2() noexcept {
    vec vals(3);
    vals.push_back(1);
    vals.push_back(2);
    vals.push_back(3);

    for (int i = 0; i < vals.length; i++) {
        vals[i] *= 2;
    }

    for (int i = 0; i < vals.length; i++) {
        vals[i] *= 4;
    }

    return vals;
}

int main() {
    auto val = test2();
    printf("%d\n", val[0]);
}