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
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <algorithm>

#define json_assert(cond, message) if(!(cond)) throw std::runtime_error(message);
#define json_assertx(cond, format, ...) if(!(cond)) throw std::runtime_error(xsnprintf(128, format, __VA_ARGS__));

#define for_each(container_type, container_obj, iterator_name) \
    for (container_type::iterator iterator_name = container_obj.begin(); iterator_name != container_obj.end(); ++iterator_name)

#define for_each_c(container_type, container_obj, iterator_name) \
    for (container_type::const_iterator iterator_name = container_obj.begin(); iterator_name != container_obj.end(); ++iterator_name)

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
    class make_map {
    private:
        std::map<K, V> map;
    public:
        make_map(const K& key, const V& val) {
            map[key] = val;
        }

        make_map<K, V>& operator()(const K& key, const V& val) {
            map[key] = val;
            return *this;
        }

        operator std::map<K, V>() {
            return map;
        }
    };

    template<typename T>
    class make_vector {
    public:
        make_vector(const T& t) {
            v.push_back(t);
        }

        make_vector& operator()(const T& t) {
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
    namespace lexer {
        typedef int RuleId;
        typedef int TokenId;
        typedef int Increment;

        const RuleId Initial = 0;
        const TokenId Invalid = ~0;
        const TokenId Skip = ~0 - 1;

        struct Transition {
            RuleId ruleId;
            bool putChar;
            TokenId tokenId;
            Increment increment;

            Transition(RuleId ruleId = Initial, bool putChar = true, TokenId tokenId = Invalid, Increment increment = 1) :
                    ruleId(ruleId), putChar(putChar), tokenId(tokenId), increment(increment) {}

            friend bool operator==(Transition const& lhs, Transition const& rhs) {
                return lhs.ruleId == rhs.ruleId &&
                       lhs.putChar == rhs.putChar &&
                       lhs.tokenId == rhs.tokenId &&
                       lhs.increment == rhs.increment;
            }

            friend bool operator!=(Transition const& lhs, Transition const& rhs) {
                return !(rhs == lhs);
            }

            friend std::ostream& operator<<(std::ostream& os, const Transition& transition) {
                os << "["
                   << transition.ruleId
                   << ", " << transition.putChar
                   << ", " << transition.tokenId
                   << ", " << transition.increment
                   << "]";
                return os;
            }
        };

        struct Condition {
            std::string chars;
            bool inSet;

            Condition(std::string const& chars = "", bool inSet = true) : chars(chars), inSet(inSet) {}

            friend bool operator<(Condition const& lhs, Condition const& rhs) {
                if (lhs.chars < rhs.chars)
                    return true;
                if (rhs.chars < lhs.chars)
                    return false;
                return lhs.inSet < rhs.inSet;
            }
        };

        struct Token {
            Token(TokenId tokenId = Invalid, std::string const& value = std::string(),
                  int startLine = -1, int endLine = -1, int startSymbol = -1, int endSymbol = -1)
                    : tokenId(tokenId), value(value),
                      startLine(startLine), endLine(endLine), startSymbol(startSymbol), endSymbol(endSymbol) {}

            friend std::ostream& operator<<(std::ostream& os, Token const& token) {
                return os << "("
                          << std::setw(3) << token.startLine << ", "
                          << std::setw(3) << token.endLine << ", "
                          << std::setw(3) << token.startSymbol << ", "
                          << std::setw(3) << token.endSymbol << "), "
                          << "tokenId: " << token.tokenId << ", "
                          << (token.value.empty() ? "" : " value: " + token.value)
                          ;
            }

            TokenId tokenId;
            std::string value;
            int startLine, endLine, startSymbol, endSymbol;
        };

        typedef std::vector<Token> Tokens;

        typedef std::map<Condition, Transition> LexerRule;
        typedef std::map<RuleId, LexerRule> LexerRules;

        typedef std::vector<std::vector<Transition> > LexerStateMachine;

        class Lexer {
        public:
            Lexer(LexerRules const &lexerRules) {
                lexerStateMachine.resize(lexerRules.size());

                for_each_c (LexerRules, lexerRules, rule) {
                    std::vector<Transition> lexerState;

                    lexerState.resize(static_cast<size_t>(std::numeric_limits<char>::max()));

                    for_each_c (LexerRule, rule->second, transition) {
                        if (transition->first.inSet) {
                            for_each_c (std::string, transition->first.chars, c) {
                                Transition &state = lexerState.at(*c);
                                if (state != Transition())
                                    throw std::runtime_error("lexer rule is ambiguous");
                                if (*c > lexerState.size())
                                    throw std::runtime_error("invalid data tokenId");
                                state = transition->second;
                            }
                        } else {
                            for (char c = 0; c < std::numeric_limits<char>::max(); ++c) {
                                std::string::iterator character = std::find(transition->first.chars.begin(),
                                                                            transition->first.chars.end(), c);
                                if (character == transition->first.chars.end()) {
                                    size_t n = static_cast<size_t>(c);
                                    if (lexerState.at(n) != Transition())
                                        throw std::runtime_error("lexer rule is ambiguous");
                                    lexerState.at(n) = transition->second;
                                }
                            }
                        }
                    }

                    lexerStateMachine.at(rule->first) = lexerState;
                }
            }

            Tokens analize(std::string const& string) {
                int currentState = 0;
                Tokens tokens;
                std::string token;
                int line = 1;
                int column = 0;
                int prevLine = line;
                int prevColumn = column;
                for (std::string::const_iterator c = string.begin(); c != string.end();) {
                    if (*c == '\n') {
                        ++line;
                        column = 0;
                    } else {
                        ++column;
                    }
                    Transition lexerState = lexerStateMachine.
                            at(static_cast<size_t>(currentState)).
                            at(static_cast<size_t>(*c));

                    if (lexerState.putChar)
                        token.push_back(*c);
                    if (lexerState.ruleId == -1)
                        throw std::runtime_error(common::xsnprintf(64, "invalid char \"%c\" at %d:%d", *c, line, column));
                    else if (lexerState.tokenId == Skip) {
                        token.clear();
                        prevLine = line;
                        prevColumn = column;
                    } else {
                        if (lexerState.ruleId == Initial) {
                            tokens.push_back(Token(static_cast<TokenId>(lexerState.tokenId), token, prevLine, line, prevColumn, column));
                            prevLine = line;
                            prevColumn = column;
                            token.clear();
                        }
                        currentState = lexerState.ruleId;
                    }
                    c += lexerState.increment;
                }
                return tokens;
            }

            friend std::ostream& operator<<(std::ostream& os, Lexer const& lexer) {
                os << "LexerStateMachine: \n";
                for_each_c (LexerStateMachine, lexer.lexerStateMachine, lexerState) {
                    if (lexerState != lexer.lexerStateMachine.begin()) os << "\n";
                    for_each_c (std::vector<Transition>, (*lexerState), it) {
                        if (it != (*lexerState).begin()) os << ", ";
                        os << *it;
                    }
                }
                os << "\n";
                return os;
            }

        private:
            LexerStateMachine lexerStateMachine;
        };
    }

    namespace rules {
        enum TokenType {
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
        };
        enum LexerStateRule {
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
        };

        typedef lexer::LexerRules LexerRules;
        typedef lexer::Condition Condition;
        typedef lexer::Transition Transition;

        typedef common::make_map<lexer::RuleId, lexer::LexerRule> MakeLexerRules;
        typedef common::make_map<lexer::Condition, lexer::Transition> MakeLexerRule;

        static LexerRules lexerRules = MakeLexerRules
                (Initial, MakeLexerRule
                        (Condition("{"), Transition(Initial, ObjectStart))
                        (Condition("}"), Transition(Initial, ObjectEnd))
                        (Condition("["), Transition(Initial, ArrayStart))
                        (Condition("]"), Transition(Initial, ArrayEnd))
                        (Condition(":"), Transition(Initial, Semicolon))
                        (Condition(","), Transition(Initial, Comma))
                        (Condition("\""), Transition(StringState, lexer::Invalid, false))
                        (Condition("+-0123456789"), Transition(IntegerState))
                        (Condition("."), Transition(FloatState))
                        (Condition("tT"), Transition(BoolState_t))
                        (Condition("fF"), Transition(BoolState_f))
                        (Condition(" "), Transition(Initial, lexer::Skip, false))
                        (Condition("\n"), Transition(Initial, lexer::Skip, false))
                        (Condition("\t"), Transition(Initial, lexer::Skip, false))
                )
                (StringState, MakeLexerRule
                        (Condition("\"", false), Transition(StringState))
                        (Condition("\""), Transition(Initial, String, false))
                )
                (IntegerState, MakeLexerRule
                        (Condition("0123456789"), Transition(IntegerState))
                        (Condition(".eE"), Transition(FloatState))
                        (Condition("0123456789.eE", false), Transition(Initial, Integer, false, 0))
                )
                (FloatState, MakeLexerRule
                        (Condition("0123456789.eE+-"), Transition(FloatState))
                        (Condition("0123456789.eE+-", false), Transition(Initial, Float, false, 0))
                )
                (BoolState_t, MakeLexerRule
                        (Condition("rR"), Transition(BoolState_r))
                )
                (BoolState_r, MakeLexerRule
                        (Condition("uU"), Transition(BoolState_u))
                )
                (BoolState_u, MakeLexerRule
                        (Condition("eE"), Transition(Initial, Bool))
                )
                (BoolState_f, MakeLexerRule
                        (Condition("aA"), Transition(BoolState_a))
                )
                (BoolState_a, MakeLexerRule
                        (Condition("lL"), Transition(BoolState_l))
                )
                (BoolState_l, MakeLexerRule
                        (Condition("sS"), Transition(BoolState_s))
                )
                (BoolState_s, MakeLexerRule
                        (Condition("eE"), Transition(Initial, Bool))
                )
        ;
    }
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

        typedef common::make_map<NonTerminals, Variants> CreateRules;
        typedef common::make_vector<Items> CreateItems;
        typedef common::make_vector<int> CreateVariant;

        static Rules jsonGrammarRules;

        States actionTable;
        int goToTable;

        void buildActionAndGotoTables() {
        }

        void parse(lexer::Tokens const& tokens) {
            std::vector<int> stack;
            for (lexer::Tokens::const_iterator tokenIt = tokens.begin(); tokenIt != tokens.end(); ++tokenIt) {
                ActionState actionState = action(*tokenIt);
                if (actionState.action == Shift) {

                } else if (actionState.action == Reduce) {

                } else if (actionState.action == Error) {

                } else if (actionState.action == Accept) {

                }
            }
        }

        ActionState action(lexer::Token const& token) {
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
                    (CreateVariant(rules::ObjectStart)(Records)(rules::ObjectEnd))
                    (CreateVariant(rules::ObjectStart)(rules::ObjectEnd))
            )
            (Array, CreateItems
                    (CreateVariant(rules::ArrayStart)(Values)(rules::ArrayEnd))
                    (CreateVariant(rules::ArrayStart)(rules::ArrayEnd))
            )
            (Records, CreateItems
                    (CreateVariant(Record)(rules::Comma)(Records))
                    (CreateVariant(Record))
            )
            (Record, CreateItems
                    (CreateVariant(rules::String)(rules::Semicolon)(Value))
            )
            (Values, CreateItems
                    (CreateVariant(Value)(rules::Comma)(Values))
                    (CreateVariant(Value))
            )
            (Value, CreateItems
                    (CreateVariant(Object))
                    (CreateVariant(Array))
                    (CreateVariant(rules::String))
                    (CreateVariant(rules::Float))
                    (CreateVariant(rules::Integer))
                    (CreateVariant(rules::Bool))
            )
    ;

    class Type {
    public:
        Type() {}

        virtual Type* clone() const = 0;

        virtual std::string stringify() const = 0;
    };

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

    class Float : public Type {
    public:
        Float(double value) : value(value){}
        virtual Type* clone() const
        {
            return new Float(*this);
        }
        virtual std::string stringify() const {
            std::stringstream stream;
            stream << value;
            return stream.str();
        }
        double value;
    };

    class Integer : public Type {
    public:
        Integer(int64_t value) : value(value){}
        virtual Type* clone() const
        {
            return new Integer(*this);
        }
        virtual std::string stringify() const {
            std::stringstream stream;
            stream << value;
            return stream.str();
        }
        int64_t value;
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

    class Json {
    public:
        Json() {}

        bool hasObject() const {
            return false;
        }

        bool hasArray() const {
            return false;
        }

        Object const &getObject() const {
        }

        Array const &getArray() const {
        }
    };

    lexer::Lexer jsonLexer(rules::lexerRules);

    // lexer::Lexer jsonLexer(jsonLexerRules);
    // parser::Parser jsonParser(jsonGrammarRules);

    Json parse(std::string const& string) {
        lexer::Tokens tokens = jsonLexer.analize(string);

        // jsonParser.parse(tokens);

        for (lexer::Tokens::const_iterator it = tokens.begin(); it != tokens.end(); ++it) {
            std::cout << *it << std::endl;
        }

        return Json();
    }
}

int main() {
    try {
        const char* js1 = "{}";
        const char* js2 = "{\"test\": 42}";
        const char* js3 = "{"
                "\"Pasha\": \"Xyu\", \n"
                "\"Pi\": 3.1415, \n"
                "\"meaningOfLife\": 42, \n"
                "\"FuckingString\": \"Tra ta ta\", \n"
                "\"MyHeartIsBroken\": True, \n"
                "\t\"AllBad\": False\n"
                "}";
        const char *js4 = "{\n"
                "\t\"Integer\": 42,\n"
                "\t\"Integer\": +42,\n"
                "\t\"Integer\": -42,\n"
                "\t\"Float\": 3.1415,\n"
                "\t\"Float\": .14e5,\n"
                "\t\"Float\": 6.67-e11,\n"
                "\t\"Float\": 2.71828,\n"
                "\t\"String\": \"String\",\n"
                "\t\"BoolFalse\": false,\n"
                "\t\"BoolTrue\": True,\n"
                "\t\"Object\": {\"TestInObject\": 52},\n"
                "\t\"Array\": [\"TestInArray\"]\n"
                "}\n";

        std::cout << json::jsonLexer << std::endl;
        json::Json js = json::parse(js3);

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