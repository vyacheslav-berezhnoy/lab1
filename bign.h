#ifndef BIGN_H
#define BIGN_H
#include <string>
#include <vector>
#include <cstdint>
class BigNum {
    private: 
        std::vector<uint8_t> num;
    public:
        BigNum() {
            num.push_back(0);
        }
        BigNum(std::string str) {
            int sz = str.size();
            for (int i = sz - 1; i >= 0; --i) {
                num.push_back(str[i] - '0');
            }
        }
        size_t size() const {
            return num.size();
        }
        BigNum(uint32_t x) {

            while (x) {
                num.push_back(x % 10);
                x/=10;
            }
        }
        operator std::string() {
            std::string s;
            int n = size();
            for (int i = n - 1; i >= 0; --i) {
                s.push_back(num[i] + '0');
            }
            return s;
        }
        BigNum& operator=(const BigNum& x) {
            if (&x == this) {
                return *this;
            }
            num = x.num;
            return *this;
        }
        friend BigNum operator+(const BigNum& x, const BigNum& y);
        friend BigNum operator*(const BigNum& x, const BigNum& y);
};
BigNum operator+(const BigNum& x, const BigNum& y) {
    int n, m;
    uint8_t sum;
    bool c;
    m = x.size();
    n = y.size();
    if (m < n) {
        return y + x;
    }
    std::vector<uint8_t> z = x.num;
    c = 0;
    for (int i = 0; i < n; i++) {
        sum = x.num[i] + y.num[i] + c;
        z[i] = sum % 10;
        c = sum / 10;
    }
    
    for (int i = n; i < m && c; i++) {
        sum = x.num[i] + 1;
        c = sum / 10;
        z[i] = sum % 10;
    }
    if (c) {
        z.push_back(1);
    }
    BigNum w;
    w.num = z;
    return w;
}
BigNum operator*(const BigNum& x, const BigNum& y) {
    int n, m, c;
    n = x.size();
    m = y.size();
    int base = 10;
    uint16_t s;
    std::vector<uint8_t> z(n+m, 0);

    for (int j = 0; j < n; ++j) {
      c = 0;
      for (int i = 0; i < m; ++i) { 
        s = x.num[j] * y.num[i] + z[i + j] + c;

        c = s / base;
        z[i+j] = s % base;
      }
      z[j + m] = c;
    }
    while(z[z.size() - 1] == 0) {
        z.pop_back();
    }
    BigNum w;
    w.num = z;
    return w;
}
BigNum exp(const BigNum&x, int p) {
    if(p==1)
        return x;
    if(p%2==1)
        return x*exp(x, p-1);
    BigNum tmp_exp = exp(x, p/2);
    return tmp_exp * tmp_exp;
}
#endif
