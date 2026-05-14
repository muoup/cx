```c++

struct affine<T> : @copy_traits(T) {
    T value;
    
    comptime int _mut_refs;
    comptime int _immut_refs;

    const T &<'a> imm<'a>(*this) 
    where
        pre: (this._mut_refs == 0)
        <'a::~(this)>: this._immut_refs--;
    {
        this._immut_refs++;
        return value;
    }

    T &<'a> mut<'a>(*this) 
    where
        pre: (this._mut_refs == 0 && this._immut_refs == 0)
        <'a::~(this)>: this._mut_refs--;
    {
        this._mut_refs++;
        return value;
    }

    <move>: where (this._mut_refs == 0 && this._immut_refs == 0)
}

```