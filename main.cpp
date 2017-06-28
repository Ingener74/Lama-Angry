#include <stdint.h>
#include <stdarg.h>
#include <set>
#include <map>
#include <string>
#include <limits>
#include <vector>
#include <cstdio>
#include <cstring>
#include <sstream>
#include <iostream>
#include <stdexcept>
#include <algorithm>

#define json_assert(cond, message) if(!(cond)) throw std::runtime_error(message);
#define json_assertx(cond, format, ...) if(!(cond)) throw std::runtime_error(xsnprintf(128, format, __VA_ARGS__));

namespace common {
    typedef std::vector<char> ByteBuffer;

    template<typename T>
    class clean_allocator {
    public :
        typedef T value_type;
        typedef value_type* pointer;
        typedef const value_type* const_pointer;
        typedef value_type& reference;
        typedef const value_type& const_reference;
        typedef std::size_t size_type;
        typedef std::ptrdiff_t difference_type;
    public :

        template<typename U>
        struct rebind {
            typedef clean_allocator<U> other;
        };

    public :
        clean_allocator() {}
        inline ~clean_allocator() {}
        clean_allocator(clean_allocator const&) {}
        template<typename U>
        clean_allocator(clean_allocator<U> const&) {}

        pointer address(reference r) { return &r; }
        const_pointer address(const_reference r) { return &r; }

        pointer allocate(size_type cnt, typename std::allocator<void>::const_pointer = 0) {
            pointer new_memory = reinterpret_cast<pointer>(::operator new(cnt * sizeof (T)));
            memset(new_memory, 0, cnt * sizeof(T));
            return new_memory;
        }
        void deallocate(pointer p, size_type n) {
            ::operator delete(p);
        }
        //    size
        size_type max_size() const {
            return std::numeric_limits<size_type>::max() / sizeof(T);
        }

        void construct(pointer p, const T& t) {
            new(p) T(t);
        }
        void destroy(pointer p) {
            p->~T();
        }

        bool operator==(clean_allocator const&) { return true; }
        bool operator!=(clean_allocator const& a) { return !operator==(a); }
    };

    template <typename K, typename V>
    class CreateMap {
    private:
        std::map<K, V> map;
    public:
        CreateMap(const K& key, const V& val) {
            map[key] = val;
        }

        CreateMap<K, V>& operator()(const K& key, const V& val) {
            map[key] = val;
            return *this;
        }

        operator std::map<K, V>() {
            return map;
        }
    };

    template <typename V>
    class CreateSet {
    private:
        std::set<V> set;
    public:
        CreateSet(const V& val) {
            set.insert(val);
        }

        CreateSet<V>& operator()(const V& val) {
            set.insert(val);
            return *this;
        }

        operator std::set<V>() {
            return set;
        }
    };

    template<typename T>
    class CreateVector {
    public:
        CreateVector(const T& t) {
            v.push_back(t);
        }

        CreateVector& operator()(const T& t) {
            v.push_back(t);
            return *this;
        }

        operator std::vector<T>() const {
            return v;
        }

        std::vector<T> v;
    };

    ByteBuffer vxsnprintf(size_t maxlen, const char* format, ...) {
        std::vector<char> buffer(maxlen);
        va_list args;
        va_start(args, format);
        vsnprintf(buffer.data(), maxlen, format, args);
        va_end(args);
        return buffer;
    }

    std::string xsnprintf(size_t maxlen, const char* format, ...) {
        va_list args;
        va_start(args, format);
        std::vector<char> buffer = vxsnprintf(maxlen, format, args);
        va_end(args);
        return std::string(buffer.data());
    }

    std::vector<std::string> splitString(const std::string& string, char delim) {
        std::stringstream stream(string);
        std::string token;
        std::vector<std::string> tokens;
        while (getline(stream, token, delim))
            tokens.push_back(token);
        return tokens;
    }

    template<typename T>
    class XPtr {
    public:
        XPtr(T* t = NULL) : t(t) {}
        ~XPtr() {
            if (t)
                delete t;
            t = NULL;
        }
        T* operator->() { return t ? t : throw std::runtime_error("pointer is null"); }
        const T* operator->() const { return t ? t : throw std::runtime_error("pointer is null"); }
        operator T*() { return t; }
        template<typename U>
        U* dcast() {
            return dynamic_cast<U*>(t);
        }
        XPtr(const XPtr& rhs) { *this = rhs; }
        XPtr& operator=(const XPtr& rhs) {
            if (this != &rhs) {
                if (t) {
                    delete t;
                    t = NULL;
                }
                t = rhs.t->clone();
            }
            return *this;
        }
    private:
        T* t;
    };
}

namespace json {

    bool any_of(const std::vector<char>& symbols, char sym) {
        std::vector<char>::const_iterator it = std::find(symbols.begin(), symbols.end(), sym);
        return it != symbols.end();
    }

    bool all_of(const std::vector<char>& symbols, const std::string& string) {
        bool result = !string.empty();
        for (std::string::const_iterator ch = string.begin(); ch != string.end(); ++ch) {
            result &= any_of(symbols, *ch);
        }
        return result;
    }

    bool isTrue(const std::string& string) {
        if (string.empty() || string.size() != 4)
            return false;
        return
                (string.at(0) == 'T' || string.at(0) == 't') &&
                (string.at(1) == 'R' || string.at(1) == 'r') &&
                (string.at(2) == 'U' || string.at(2) == 'u') &&
                (string.at(3) == 'E' || string.at(3) == 'e');
    }
    bool isFalse(const std::string& string) {
        if (string.empty() || string.size() != 5)
            return false;
        return
                (string.at(0) == 'F' || string.at(0) == 'f') &&
                (string.at(1) == 'A' || string.at(1) == 'a') &&
                (string.at(2) == 'L' || string.at(2) == 'l') &&
                (string.at(3) == 'S' || string.at(3) == 's') &&
                (string.at(4) == 'E' || string.at(4) == 'e');
    }

    typedef common::CreateVector<char> CreateSymTable;
    static std::vector<char> integer_ = CreateSymTable('0')('1')('2')('3')('4')('5')('6')('7')('8')('9')('-');
    static std::vector<char> float_ = CreateSymTable('0')('1')('2')('3')('4')('5')('6')('7')('8')('9')('.')('e')('E')('+')('-');
    static std::vector<char> true_ = CreateSymTable('t')('T')('r')('R')('u')('U')('e')('E');
    static std::vector<char> false_ = CreateSymTable('f')('F')('a')('A')('l')('L')('s')('S')('e')('E');

    struct Token {
        enum Type {
            ObjectStart,
            ObjectEnd,
            ArrayStart,
            ArrayEnd,
            Semicolon,
            Comma,
            Integer,
            Float,
            String,
            Bool,

			TypeCount,
        };

        Token(Type type, const std::string& value = std::string()) : type(type), value(value) {}

        friend std::ostream& operator<<(std::ostream& os, const Token& token) {
            static std::map<Type, std::string> tokenTypes =
                    common::CreateMap<Type, std::string>
                            (ObjectStart, "{")
                            (ObjectEnd, "}")
                            (ArrayStart, "[")
                            (ArrayEnd, "]")
                            (Semicolon, ":")
                            (Comma, ",")
                            (Integer, "Integer")
                            (Float, "Float")
                            (String, "String")
                            (Bool, "Bool");
            return os << "type: " << tokenTypes.at(token.type) << (token.value.empty() ? "" : " value: " + token.value);
        }

		enum LexerState {
			Initial,
			IntegerState,
			FloatState,
			StringState,
			BoolState_t,
			BoolState_r,
			BoolState_u,
			BoolState_f,
			BoolState_a,
			BoolState_l,
			BoolState_s,
			BoolState_e,
			EndToken,
		};

		struct LexerOutput {
			LexerState lexerState;
			Type type;

			LexerOutput(LexerState lexerState = Initial, Type type = TypeCount) : lexerState(lexerState), type(type)
			{}
		};
		enum Condition {
			Equal,
			NotEqual,
			InSet,
			NotInSet,
		};
		struct Character {
			std::set<char> chars;
			Condition condition;

			Character(const std::set<char>& chars = std::set<char>(), Condition condition = Equal) : chars(chars), condition(condition)
			{}

			friend bool operator<(const Character& lhs, const Character& rhs)
			{
				if (lhs.chars < rhs.chars)
					return true;
				if (rhs.chars < lhs.chars)
					return false;
				return lhs.condition < rhs.condition;
			}
		};
        typedef std::map<LexerState, std::map<Character, LexerOutput> > LexerStateMachine;
		typedef common::CreateMap<LexerState, std::map<Character, LexerOutput> > CreateLexerStateMachine;
		typedef common::CreateMap<Character, LexerOutput> CreateLexerState;
		typedef common::CreateSet<char> CreateChars;
        static LexerStateMachine lexerStateMachine;


        typedef std::vector<Token> Tokens;

        static Tokens tokenize(const std::string& string) {
            Tokens tokens;
            int row = 0;
            int column = 0;
            for (std::string::const_iterator ch = string.begin(); ch != string.end(); ++ch) {
                if (*ch == '{') {
                    tokens.push_back(Token(Token::ObjectStart));
                } else if (*ch == '}') {
                    tokens.push_back(Token(Token::ObjectEnd));
                } else if (*ch == '[') {
                    tokens.push_back(Token(Token::ArrayStart));
                } else if (*ch == ']') {
                    tokens.push_back(Token(Token::ObjectEnd));
                } else if (*ch == ':') {
                    tokens.push_back(Token(Token::Semicolon));
                } else if (*ch == ',') {
                    tokens.push_back(Token(Token::Comma));
                } else if (*ch == '\"') {
                    std::string token;
                    while (*++ch != '\"')
                        token.push_back(*ch);
                    tokens.push_back(Token(Token::String, token));
                } else if (any_of(integer_, *ch) || any_of(float_, *ch)) {
                    std::string token;
                    Token::Type type = Token::Integer;
                    bool isInteger;
                    bool isFloat;
                    do {
                        isInteger = any_of(integer_, *ch);
                        isFloat = any_of(float_, *ch);
                        if (isInteger || isFloat)
                            token.push_back(*ch++);
                        if (isInteger)
                            continue;
                        if (isFloat)
                            type = Token::Float;
                    } while (isInteger || isFloat);
                    tokens.push_back(Token(type, token));
                    ch--;
                } else if (any_of(true_, *ch) || any_of(false_, *ch)) {
                    std::string token;
                    token.push_back(*ch++);
                    while(any_of(true_, *ch) || any_of(false_, *ch))
                        token.push_back(*ch++);
                    tokens.push_back(Token(Token::Bool, isTrue(token) ? "true" : isFalse(token) ? "false" : throw std::runtime_error("something wrong, not true and not false")));
                    ch--;
                } else if(*ch == ' ' || *ch == '\t') {
                    column++;
                } else if(*ch == '\n') {
                    column = 0;
                    row++;
                } else throw std::runtime_error(common::xsnprintf(64, "invalid symbol %d", static_cast<int>(*ch)));
            }
            return tokens;
        }

        Type type;
        std::string value;
        int startLine, endLine, startSymbol, endSymbol;
    };

	Token::LexerStateMachine Token::lexerStateMachine = Token::CreateLexerStateMachine
			(Token::Initial, Token::CreateLexerState
					(Token::Character(Token::CreateChars('{')), Token::LexerOutput(Token::EndToken, Token::ObjectStart))
					(Token::Character(Token::CreateChars('}')), Token::LexerOutput(Token::EndToken, Token::ObjectEnd))
					(Token::Character(Token::CreateChars('[')), Token::LexerOutput(Token::EndToken, Token::ArrayStart))
					(Token::Character(Token::CreateChars(']')), Token::LexerOutput(Token::EndToken, Token::ArrayEnd))
					(Token::Character(Token::CreateChars(':')), Token::LexerOutput(Token::EndToken, Token::Semicolon))
					(Token::Character(Token::CreateChars(',')), Token::LexerOutput(Token::EndToken, Token::Comma))
					(Token::Character(Token::CreateChars('"')), Token::LexerOutput(Token::StringState))
					(Token::Character(Token::CreateChars('+')('-'), Token::InSet), Token::LexerOutput(Token::IntegerState))
					(Token::Character(Token::CreateChars('t')('T'), Token::InSet), Token::LexerOutput(Token::BoolState_t))
					(Token::Character(Token::CreateChars('f')('F'), Token::InSet), Token::LexerOutput(Token::BoolState_f))

			)
			(Token::StringState, Token::CreateLexerState
					(Token::Character(Token::CreateChars('"')), Token::LexerOutput(Token::StringState))
					(Token::Character(Token::CreateChars('"'), Token::NotEqual), Token::LexerOutput(Token::StringState))
					(Token::Character(Token::CreateChars('"')), Token::LexerOutput(Token::EndToken, Token::String))
			)
			(Token::IntegerState, Token::CreateLexerState
					(Token::Character(Token::CreateChars('0')('1')('2')('3')('4')('5')('6')('7')('8')('9'), Token::InSet), Token::LexerOutput(Token::IntegerState))
					(Token::Character(Token::CreateChars('0')('1')('2')('3')('4')('5')('6')('7')('8')('9'), Token::NotInSet), Token::LexerOutput(Token::EndToken, Token::Integer))
					(Token::Character(Token::CreateChars('.')('e')('E'), Token::InSet), Token::LexerOutput(Token::FloatState))
			)
			(Token::BoolState_t, Token::CreateLexerState
					(Token::Character(Token::CreateChars('r')('R'), Token::InSet), Token::LexerOutput(Token::BoolState_r))
			)
			(Token::BoolState_r, Token::CreateLexerState
					(Token::Character(Token::CreateChars('u')('U'), Token::InSet), Token::LexerOutput(Token::BoolState_u))
			)
			(Token::BoolState_u, Token::CreateLexerState
					(Token::Character(Token::CreateChars('u')('U'), Token::InSet), Token::LexerOutput(Token::BoolState_e))
			)
			(Token::BoolState_e, Token::CreateLexerState
					(Token::Character(Token::CreateChars('e')('E'), Token::InSet), Token::LexerOutput(Token::EndToken, Token::Bool))
			)
			(Token::BoolState_f, Token::CreateLexerState
					(Token::Character(Token::CreateChars('a')('A'), Token::InSet), Token::LexerOutput(Token::BoolState_a))
			)
			(Token::BoolState_a, Token::CreateLexerState
					(Token::Character(Token::CreateChars('l')('L'), Token::InSet), Token::LexerOutput(Token::BoolState_l))
			)
			(Token::BoolState_l, Token::CreateLexerState
					(Token::Character(Token::CreateChars('s')('S'), Token::InSet), Token::LexerOutput(Token::BoolState_e))
			)
	;

/**
 * Json grammar rules description
 *
 * Json    ::= Object | Array
 * Object  ::= ObjectStart, Records, ObjectEnd | ObjectStart, ObjectEnd
 * Array   ::= ArrayStart, Values, ArrayEnd | ArrayStart, ArrayEnd
 * Values  ::= Value, Comma, Values | Value
 * Value   ::= Integer | String | FLoat | Bool | Array | Object
 * Records ::= Record, Comma, Records | Record
 * Record  ::= String, Semicolon, Value
 */

    struct Parser {
        enum NonTerminals {
            Json = 100,
            Object,
            Array,
            Records,
            Record,
            Values,
            Value,

            NonTerminalsCount,
        };

        enum Action {
            Shift,
            Reduce,
            Error,
            Accept,
        };

        struct ActionState {
            Action action;
            int state;

            ActionState(Action action, int state) : action(action), state(state){}
        };

        typedef std::vector<std::vector<ActionState> > States;

        typedef std::vector<int> Items;
        typedef std::vector<Items> Variants;
        typedef std::map<NonTerminals, Variants> Rules;

        typedef common::CreateMap<NonTerminals, Variants> CreateRules;
        typedef common::CreateVector<Items> CreateItems;
        typedef common::CreateVector<int> CreateVariant;

        static Rules jsonGrammarRules;

        States actionTable;
        int goToTable;

        void buildActionAndGotoTables() {
        }

        void parse(Token::Tokens const& tokens) {
            std::vector<int> stack;
            for (Token::Tokens::const_iterator tokenIt = tokens.begin(); tokenIt != tokens.end(); ++tokenIt) {
                ActionState actionState = action(*tokenIt);
                if (actionState.action == Shift) {

                } else if (actionState.action == Reduce) {

                } else if (actionState.action == Error) {

                } else if (actionState.action == Accept) {

                }
            }
        }

        ActionState action(Token const& token) {
            return ActionState(Shift, 0);
        }

        int goTo() {
        }
    };

    Parser::Rules Parser::jsonGrammarRules = CreateRules
            (Json, CreateItems
                    (CreateVariant(Object))
                    (CreateVariant(Array))
            )
            (Object, CreateItems
                    (CreateVariant(Token::ObjectStart)(Records)(Token::ObjectEnd))
                    (CreateVariant(Token::ObjectStart)(Token::ObjectEnd))
            )
            (Array, CreateItems
                    (CreateVariant(Token::ArrayStart)(Values)(Token::ArrayEnd))
                    (CreateVariant(Token::ArrayStart)(Token::ArrayEnd))
            )
            (Records, CreateItems
                    (CreateVariant(Record)(Token::Comma)(Records))
                    (CreateVariant(Record))
            )
            (Record, CreateItems
                    (CreateVariant(Token::String)(Token::Semicolon)(Value))
            )
            (Values, CreateItems
                    (CreateVariant(Value)(Token::Comma)(Values))
                    (CreateVariant(Value))
            )
            (Value, CreateItems
                    (CreateVariant(Object))
                    (CreateVariant(Array))
                    (CreateVariant(Token::String))
                    (CreateVariant(Token::Float))
                    (CreateVariant(Token::Integer))
                    (CreateVariant(Token::Bool))
            )
    ;

    class Type {
    public:
        Type() {}

        virtual Type* clone() const = 0;

        virtual std::string stringify() const = 0;
    };

#define JSON_TYPE(Primary, secondary)           \
class Primary : public Type {                   \
public:                                         \
    Primary(secondary value) : value(value){}   \
    virtual Type* clone() const                 \
    {                                           \
        return new Primary(*this);              \
    }                                           \
    virtual std::string stringify() const {     \
        std::stringstream stream;               \
        stream << value;                        \
        return stream.str();                    \
    }                                           \
    secondary value;                            \
};

    JSON_TYPE(Integer, int64_t)
//JSON_TYPE(String, std::string)
    JSON_TYPE(Float, double)
//JSON_TYPE(Bool, bool)

    class String : public Type {
    public:
        String(std::string const& value) : value(value){}
        virtual Type* clone() const
        {
            return new String(*this);
        }
        virtual std::string stringify() const {
            std::stringstream stream;
            stream << "\"" << value << "\"";
            return stream.str();
        }
        std::string value;
    };

    class Bool : public Type {
    public:
        Bool(bool value) : value(value){}
        virtual Type* clone() const {
            return new Bool(*this);
        }
        virtual std::string stringify() const {
            std::stringstream stream;
            stream << (value ? "true" : "false");
            return stream.str();
        }
        bool value;
    };

    class Array;

    class Object : public Type {
    public:
        typedef std::map<std::string, common::XPtr<Type>,
        std::less<std::string>,
        common::clean_allocator<std::pair<const std::string, common::XPtr<Type> > > > Fields;
        Object() {}

        virtual Type* clone() const {
            return new Object(*this);
        }

        virtual std::string stringify() const
        {
            std::stringstream stream;
            stream << "{";
            for (Fields::const_iterator it = fields.begin(); it != fields.end(); ++it) {
                if (it != fields.begin()) {
                    stream << ", ";
                }
                stream << "\"" << it->first << "\"" << ": " << it->second->stringify();
            }
            stream << "}";
            return stream.str();
        }

        Object& operator()(const std::string& key, const std::string& v) { return add<String>(key, v); }
        Object& operator()(const std::string& key, const char* v) { return add<String>(key, v); }
        Object& operator()(const std::string& key, int64_t v) { return add<Integer>(key, v); }
        Object& operator()(const std::string& key, int v) { return add<Integer>(key, static_cast<int64_t>(v)); }
        Object& operator()(const std::string& key, bool v) { return add<Bool>(key, v); }
        Object& operator()(const std::string& key, double v) { return add<Float>(key, v); }
        Object& operator()(const std::string& key, const Object& v) { return add<Object>(key, v); }
        Object& operator()(const std::string& key, const Array& v);

        template<typename T, typename V>
        Object& add(const std::string& key, const V& v) {
            json_assert(fields.insert(std::make_pair(key, new T(v))).second, "can't insert field");
            return *this;
        };

        template <typename T>
        T* get(const std::string& key) {
            Fields::iterator it = fields.find(key);
            if (it == fields.end()) {
                return NULL;
            }
            T* t = it->second.dcast<T>();
            if (!t)
                return NULL;
            return t;
        }

        int64_t integerOr(const std::string& key, int64_t defaultValue = 0) {
            Integer* integer = get<Integer>(key);
            if (!integer)
                return defaultValue;
            return integer->value;
        }

        friend std::ostream& operator<<(std::ostream& os, const Object& object)
        {
            return os << object.stringify();
        }

        Fields fields;
    };

    class Array : public Type {
    public:
        typedef std::vector<common::XPtr<Type>, common::clean_allocator<common::XPtr<Type> > > Fields;
        Array() {}

        virtual Array* clone() const {
            return new Array(*this);
        }

        Array& operator()(const std::string& v) { return add<String>(v); }
        Array& operator()(const char* v) { return add<String>(v); }
        Array& operator()(int64_t v) { return add<Integer>(v); }
        Array& operator()(int v) { return add<Integer>(static_cast<int64_t>(v)); }
        Array& operator()(bool v) { return add<Bool>(v); }
        Array& operator()(double v) { return add<Float>(v); }
        Array& operator()(const Object& v) { return add<Object>(v); }
        Array& operator()(const Array& v) { return add<Array>(v); }

        template<typename T, typename V>
        Array& add(const V& v) {
            fields.push_back(new T(v));
            return *this;
        };

        template <typename T>
        T* get(size_t index) {
            T* t = fields.at(index).dcast<T>();
            if (!t)
                return NULL;
            return t;
        }

        int64_t integerOr(size_t index, int64_t defaultValue = 0) {
            Integer* integer = get<Integer>(index);
            if (!integer)
                return defaultValue;
            return integer->value;
        }

        virtual std::string stringify() const {
            std::stringstream stream;
            stream << "[";
            for (Fields::const_iterator it = fields.begin(); it != fields.end(); ++it) {
                if (it != fields.begin())
                    stream << ", ";
                stream << (*it)->stringify();
            }
            stream << "]";
            std::string string = stream.str();
            return string;
        }

        friend std::ostream& operator<<(std::ostream& os, const Array& array)
        {
            return os << array.stringify();
        }

        Fields fields;
    };

    Object& Object::operator()(const std::string& key, const Array& v)
    {
        return add<Array>(key, v);
    }

    void jsonParse(const char* json) {
    }

}

int main() {
    try {
        typedef std::vector<json::Token> Tokens;

        const char* test_json = "{"
            "\"Pasha\": \"Xyu\", \n"
            "\"Pi\": 3.1415, \n"
            "\"meaningOfLife\": 42, \n"
            "\"FuckingString\": \"Tra ta ta\", \n"
            "\"MyHeartIsBroken\": True, \n"
            "\t\"AllBad\": False\n"
        "}";

        Tokens tokens = json::Token::tokenize(test_json);

        for (Tokens::iterator it = tokens.begin(); it != tokens.end(); ++it) {
            std::cout << *it << std::endl;
        }

        json::Parser().parse(tokens);

        std::cout << json::Object()
                ("Test String", "string")
                ("Test object", json::Object()
                        ("Foo", 32)
                        ("Bar", json::Array()
                                (564)
                                ("Test")
                        )
                        ("Quuz", json::Array()
                                (42)
                                (false)
                        )
                )
                ("Test Integer", 42)
                ("Test Float", 3.1415)
                ("Test Bool", false) << std::endl;

        std::cout << std::endl;
   }
    catch (std::exception const& e) {
        std::cerr << e.what() << std::endl;
    }
}