#include <string>
#include <vector>
#include <cmath>
#include <cstdint>
#include <limits>
#include <iostream>
#include <cctype>
#include <algorithm>
#include "bign.h"
int nlz(unsigned int);
void strip_zeros(std::vector<unsigned int> &x);
void bitnot(std::vector<unsigned int> &x);
void div_rem(std::vector<unsigned int> &quot,
               std::vector<unsigned int> &u, std::vector<unsigned int> v);
uint32_t string_rem(std::string &str, const uint64_t base);
int nlz(unsigned x) {
  unsigned y;
  int n;
  n = 32;
  y = x >>16; if (y != 0) {n = n -16; x = y;}
  y = x >> 8; if (y != 0) {n = n - 8; x = y;}
  y = x >> 4; if (y != 0) {n = n - 4; x = y;}
  y = x >> 2; if (y != 0) {n = n - 2; x = y;}
  y = x >> 1; if (y != 0) return n - 2;
  return n - x;
}
class BigInt {
    private:
    std::vector<unsigned int> digits;
    bool sign; // false if positive true if negative
 public:
  BigInt() {
    sign = false;
    digits.push_back(0);
  }
  BigInt(int x) {
    digits.clear();
    if (x < 0) {
      sign = true;
      digits.push_back(-x);
    }
    else {
      sign = false;
      digits.push_back(x);
    }
  }
  BigInt(std::string str) {
    if (str == "0") {
      digits.push_back(0);
      return;
    }
    if (str[0] == '-') {
      str = str.substr(1);
      sign = true;
    }
    else {
      sign = false;
    }

    if (!(std::all_of(str.begin(), str.end(), ::isdigit))) {
      throw std::invalid_argument("Not a valid integer string!");
    }; //exception

    const uint64_t base = 4294967296;
    while (str.size() > 17) {
      digits.push_back(string_rem(str, base));
    }
    uint64_t slice = std::stoull(str);
    while (slice) {
      digits.push_back(slice % base);
      slice /= base;
    }

  } // бросать исключение std::invalid_argument при ошибке

  BigInt(const BigInt&x) {
    digits = x.digits;
    sign = x.sign;
  }
  BigInt& operator=(const BigInt&x) {
    if (&x == this) {
      return *this;
    }
    digits = x.digits;
    sign = x.sign;
    return *this;
  }

  BigInt operator~() const {
    BigInt temp = *this;
    temp++;
    temp.sign = !sign;
    return temp;
  }

  BigInt& operator++() {
    return (*this) += BigInt(1);
  }
  const BigInt operator++(int)  {
    const BigInt temp = *this;
    ++(*this);
    return temp;
  }
  BigInt& operator--() {
    return (*this) += BigInt(-1);
  }
  const BigInt operator--(int) {
    const BigInt temp = *this;
    --(*this);
    return temp;
  }

  BigInt& operator+=(const BigInt&x) {
    int usize, vsize;
    usize = digit_num();
    vsize = x.digit_num();

    if (usize < vsize) {
        BigInt temp = *this;
        *this = x;
        return *this += temp;
    }

    if (sign ^ x.sign) {
        /*Signs are different. We need to subtract*/
        if (*this < x) {
          BigInt temp = -(*this);
          if (temp > x) {
            us_diff(temp, x);
            temp.sign = true;
            *this = temp;
            return *this;
          }
          else {
            BigInt temp1 = x;
            us_diff(temp1, temp);
            temp1.sign = false;
            *this = temp1;
            return *this;
          }
        }
        else {
          BigInt temp = -x;
          if (temp > *this) {
            us_diff(temp, *this);
            temp.sign = true;
            *this = temp;
            return *this;
          }
          else {
            us_diff(*this, temp);
            sign = false;
            return *this;
          }
        }
    }
    else {
      *this = us_sum(*this, x);
    }
    return *this;
  }
  BigInt& us_sum(BigInt &y, const BigInt&x) {
    unsigned int sum1, sum2, c1, c2, c;
    int n = x.digit_num();
    int m = y.digit_num();
    c = 0;
    for (int i = 0; i < n; i++) {
        sum1 = y.digits[i] + x.digits[i];
        c1 = sum1 < y.digits[i];
        sum2 = sum1 + c;
        c2 = sum2 < sum1;
        c = c1 | c2;
        y.digits[i] = sum2;
    }
    
    for (int i = n; i < m && c; i++) {
        sum1 = y.digits[i] + 1;
        c = sum1 < y.digits[i];
        y.digits[i] = sum1;
    }
    if (c) {
        y.digits.push_back(1);
    }
    return y;
  }
  BigInt& us_diff(BigInt &y, const BigInt&x) {
    unsigned int diff1, diff2, c1, c2, c;
    int n = x.digit_num();
    int m = y.digit_num();
    c = 0;
    for (int i = 0; i < n; i++) {
        diff1 = y.digits[i] - x.digits[i];
        c1 = diff1 > y.digits[i];
        diff2 = diff1 - c;
        c2 = diff2 > diff1;
        c = c1 | c2;
        y.digits[i] = diff2;
    }
    if (c) {
        y.digits[n] -= 1;
        if (m == n + 1 && !y.digits[n]) {
            y.digits.pop_back();
        }
    }
    strip_zeros(y.digits);
    return y;
  }
  BigInt& operator*=(const BigInt&x) {
    if (x == BigInt(0) || *this == BigInt(0)) {
      *this = BigInt(0);
      return *this;
    }
    uint64_t s;
    uint64_t a, b;
    unsigned int c;
    int m, n;
    m = digit_num();
    n = x.digit_num();
    std::vector<unsigned int> w(m+n, 0);
    for (int j = 0; j < n; ++j) {
      c = 0;
      for (int i = 0; i < m; ++i) {
        a = digits[i];
        b = x.digits[j];
        s = a * b;
        s = s + w[i + j] + c;
        w[i+j] = s;
        c = s >> 32;
      }
      w[j + m] = c;
    }
    sign = sign ^ x.sign;
    strip_zeros(w);
    digits = w;
    return *this;
  }
  BigInt& operator-=(const BigInt&x) {
    int usize, vsize;
    usize = digit_num();
    vsize = x.digit_num();
    if (x.sign) {
      return *this += (-x);
    }
    if (usize < vsize) {
        BigInt temp = *this;
        *this = x;
        return *this -= temp;
    }

    if (sign ^ x.sign) {
      BigInt temp = -x;
      *this += temp;
    }
    else {
      if (x > *this) {
        BigInt temp = x;
        *this = us_diff(temp, *this);
        sign = true;
      }
      else{ 
        *this = us_diff(*this, x);
      }
    }
    return *this;
  }
  BigInt& operator/=(const BigInt&x) {
    if (x == BigInt(0)) {
      throw std::runtime_error("Division by zero!");
    }
    int m, n;
    n = x.digit_num();
    m = digit_num() - n;
    if (m < 0) {
      *this = BigInt(0);
      return *this;
    }
    std::vector<uint32_t> quot(m+1, 0);
    div_rem(quot, digits, x.digits);
    sign = sign ^ x.sign;
    digits = quot;
    strip_zeros(digits);
    return *this;
  }
  BigInt& operator^=(const BigInt&x) {
    int m, n;
    m = digit_num();
    n = x.digit_num();
    if (m < n) {
      BigInt temp = (*this);
      *this = x;
      return (*this) ^= temp;
    }
    BigInt temp = x;
    bool final_sign = sign ^ temp.sign;
    if (sign) {
      sign = false;
      bitnot(digits);
      ++(*this);
    }
    if (temp.sign) {
      temp.sign = false;
      bitnot(temp.digits);
      ++temp;
      for (int i = m - 1; i != n - 1; --i) {
        digits[i] = ~digits[i];
      }
    }
    for (int i = 0; i < n; ++i) {
      digits[i] ^= temp.digits[i];
    }
    if (final_sign) {
      bitnot(digits);
      ++(*this);
    }
    sign = final_sign;
    strip_zeros(digits);
    return *this;
  }

  BigInt& operator%=(const BigInt&x) {
    if (x == BigInt(0)) {
      throw std::runtime_error("Mod 0 is not defined!");
    }
    int m, n;
    n = x.digit_num();
    m = digit_num() - n;
    if (m < 0) {
      return *this;
    }
    std::vector<uint32_t> quot(m+1, 0);
    div_rem(quot, digits, x.digits);
    if (sign ^ x.sign) {
      sign = true;
    }
    strip_zeros(digits);
    return *this;
  }
  BigInt& operator&=(const BigInt&x) {
    int m, n;
    m = digit_num();
    n = x.digit_num();
    if (m < n) {
      BigInt temp = (*this);
      *this = x;
      return (*this) &= temp;
    }
    BigInt temp = x;
    bool final_sign = sign & temp.sign;
    if (!temp.sign) {
      for (int i = m - n; i != 0; --i) {
        digits.pop_back();
      }
    }
    if (sign) {
      sign = false;
      bitnot(digits);
      ++(*this);
    }
    if (temp.sign) {
      temp.sign = false;
      bitnot(temp.digits);
      ++temp;
    }
    for (int i = 0; i < n; ++i) {
      digits[i] &= temp.digits[i];
    }
    if (final_sign) {
      bitnot(digits);
      ++(*this);
    }
    sign = final_sign;
    strip_zeros(digits);
    return *this;
  }
  BigInt& operator|=(const BigInt&x) {
    int m, n;
    m = digit_num();
    n = x.digit_num();
    if (m < n) {
      BigInt temp = (*this);
      *this = x;
      return (*this) |= temp;
    }
    BigInt temp = x;
    bool final_sign = sign | temp.sign;
    if (final_sign) {
      for (int i = m - n; i != 0; --i) {
        digits.pop_back();
      }
      if (sign) {
        sign = false;
        bitnot(digits);
        ++(*this);
      }
      if (temp.sign) {
        temp.sign = false;
        bitnot(temp.digits);
        ++temp;
      }
    }

    for (int i = 0; i < n; ++i) {
      digits[i] |= temp.digits[i];
    }
    if (final_sign) {
      bitnot(digits);
      ++(*this);
    }
    sign = final_sign;
    strip_zeros(digits);
    return *this;
  }

  BigInt operator+() const {
    return *this;
  }  // unary +
  BigInt operator-() const {
    BigInt temp = *this;
    temp.sign = !(temp.sign);
    return temp;
  }

  bool operator==(const BigInt&x) const {
    if (sign ^ x.sign) {
      return false;
    }
    if (digit_num() != x.digit_num()) {
      return false;
    }
    for (int i = digit_num() - 1; i != -1 ; i--) {
      if (digits[i] != x.digits[i]) {
        return false;
      }
    }
    return true;
  }
  bool operator!=(const BigInt&x) const {
    return !(*this == x);
  }
  bool operator<(const BigInt&x) const {
    if (sign ^ x.sign) {
      return sign;
    }
    if (digit_num() != x.digit_num()) {
      return (digit_num() < x.digit_num()) ? true : false;
    }
    for (int i = digit_num() - 1; i != -1 ; --i) {
      if (digits[i] != x.digits[i]) {
        return (digits[i] < x.digits[i]) ? true : false;
      }
    }
    return false;
  }
  bool operator>(const BigInt&x) const {
    return !(*this <= x);
  }
  bool operator<=(const BigInt&x) const {
    return (*this < x || *this == x);
  }
  bool operator>=(const BigInt&x) const {
    return !(*this < x);
  }
  friend std::ostream& operator<<(std::ostream& o, const BigInt& i);
  operator int() const {
    return sign ? -(digits[0]) : digits[0];
  }
  operator std::string() const {
    std::string base = "4294967296";
    int n = digit_num();
    if (n == 1) {
      if (sign) return "-" + std::to_string(digits[0]);
      return std::to_string(digits[0]);
    }
    BigNum b(base);
    BigNum res;
    BigNum temp;
    BigNum e = exp(b, n-1);
    res = e * BigNum(digits[n-1]);
    for (int i = n - 2; i > 0; --i) {
      if (digits[i]) {
        e = exp(b, i);
        temp = BigNum(digits[i]);
        temp = e * temp;
        res = res + temp;
      }
    }
    temp = BigNum(digits[0]);
    res = res + temp;
    if (sign) {
      return "-" + std::string(res);
    }
    return std::string(res);
  }
  int digit_num() const { //returns number of digits. negative if our integer is negative
    return digits.size();
  }
  size_t size() const {
    return digit_num() * 4;
  }  // size in bytes
  friend void div_rem(BigInt &quot, BigInt &rem, const BigInt&x, const BigInt&y);
  friend std::vector<uint32_t> complement();
};
uint32_t string_rem(std::string &str, const uint64_t base) {
    std::string sub;
    uint64_t slice;
    std::string res;
    uint32_t rem;
    std::string temp;
    if (str.size() < 11) {
      slice = std::stoull(str);
      temp = std::to_string(slice/base);
      rem = slice % base;
      str = temp;
      return rem;
    }
    sub = str.substr(0,9);
    str = str.substr(9);
    while (str.size()) {
      sub = sub + str.substr(0,1);
      str = str.substr(1);
      slice = std::stoull(sub);
      while (slice < base && str.size()) {
        res+= "0";
        sub = sub + str.substr(0,1);
        str = str.substr(1);
        slice = std::stoull(sub);
      }
      temp = std::to_string(slice/base);
      rem = slice % base;
      res += temp;
      sub = std::to_string(rem);
    }
    if (res[0] == '0') {
      res = res.substr(1);
    }
    str = res;
    return rem;
}
void strip_zeros(std::vector<unsigned int> &x) {
  for (int i = x.size() - 1; i != 0 && !x[i]; --i) {
      x.pop_back();
    }
}
void bitnot(std::vector<unsigned int> &x) {
  for (int i = 0; i < x.size(); ++i) {
    x[i] = ~x[i];
  }
}
void div_rem(std::vector<unsigned int> &quot,
               std::vector<unsigned int> &u, std::vector<unsigned int> v) {

  const uint64_t b = 4294967296; //number base 32 bits
  uint64_t qhat, rhat;
  int n, m, norm;
  n = v.size();
  m = u.size() - n;
  uint64_t p;
  int64_t t, k;
  uint64_t temp_a, temp_b;

  if (n == 1) {  
    k = 0;
    for (int i = u.size() - 1; i >= 0; --i){
      temp_a = k*b;
      temp_a += u[i];
      temp_a /= v[0];
      quot[i] = temp_a;
      k = (k*b + u[i]) % v[0];
    }
    u.clear();
    u.push_back(k);
    return;
  }
  norm = nlz(v[n-1]);
  for (int i = n - 1; i > 0; --i) { //change to >= if necessary
    v[i] =  (v[i] << norm) | (v[i-1] >> (32 - norm));
  }
  v[0] = v[0] << norm;
  u.push_back(u[m+n-1] >> (32 - norm)); //now the size of u is m+n+1
  for (int i = m + n - 1; i > 0; --i) {
    u[i] = (u[i] << norm) | (u[i-1] >> (32 - norm));
  }
  u[0] = u[0] << norm;
  for (int j = m; j>= 0; --j) {
    qhat = (u[j+n]*b + u[j+n-1]) / v[n-1];
    rhat = (u[j+n]*b + u[j+n-1]) % v[n-1];
    if ((qhat == b || qhat*v[n-2] > b*rhat + u[j+n-2])) {
      do {
        if (qhat = b || qhat*v[n-2] > b*rhat + u[j+n-2]) {
          --qhat;
          rhat+=v[n-1];
        }
      } while (rhat < b);
    }
    // Multiply and subract
    k = 0;
    for (int i = 0; i < n; ++i) {
      p = qhat*v[i];
      temp_a = u[i+j];
      t = temp_a - k - (p & 0xFFFFFFFF);
      u[i+j] = t;
      k = (p >> 32) - (t >> 32);
    }
    temp_a = u[j+n];
    t = temp_a - k;
    u[j+n] = t;
    quot[j] = qhat;
    if (t < 0) {
      quot[j] -= 1;
      k = 0;
      for (int i = 0; i < n; ++i) {
        temp_a = u[i+j];
        temp_b = v[i];
        t = temp_a + temp_b + k;
        u[i+j] = t;
        k = t >> 32;
      }
      u[j+n] += k;
    }
  }
  // unnormalisation
  for (int i = 0; i < n; i++) {
    u[i] = (u[i] >> norm) | (u[i+1] << 32 - norm);
  }
  return;
}
BigInt operator+(const BigInt&x, const BigInt&y) {
  BigInt answ = x;
  return answ += y;
}
BigInt operator-(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp -= y;
}
BigInt operator*(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp *= y;
}
BigInt operator/(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp /=y;
}
BigInt operator^(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp ^= y;
}
BigInt operator%(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp %= y;
}
BigInt operator&(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp &= y;
}
BigInt operator|(const BigInt&x, const BigInt&y) {
  BigInt temp = x;
  return temp |= y;
}


std::ostream& operator<<(std::ostream& o, const BigInt& i) {
  o << std::string(i) << std::endl;
  return o;
}

int main() {
  try {
    BigInt a("999438893498129923883");
    BigInt b("98349843989348934891");
    BigInt c = a / b;
    std::cout << c << std::endl;
    c = a % b;
    std::cout << c << std::endl;
    c = a * b;
    std::cout << c << std::endl;
    c = a ^ b;
    std::cout << c << std::endl;
    c = a & b;
    std::cout << c << std::endl;
    c = a | b;
    std::cout << c << std::endl;
    c = a + b;
    std::cout << c << std::endl;
    c = a - b;
    std::cout << c << std::endl;
    c = b - a;
    std::cout << c << std::endl;
  }
  catch (const std::runtime_error &ex) {
    std::cout << "Arithmetic error: " << ex.what() << std::endl;
  }
  catch (const std::invalid_argument &ex) {
    std::cout << "Invalid argument: " << ex.what() << std::endl;
  }
}
