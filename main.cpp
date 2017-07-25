#include <stdint.h>
#include <set>
#include <map>
#include <string>
#include <limits>
#include <vector>
#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <algorithm>

#define json_assert(cond, message) if(!(cond)) throw std::runtime_error(message);
#define json_asserte(cond, exception_class, message) if(!(cond)) throw exception_class(message);
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

        void construct(pointer p, T const& t) {
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
        make_map(K const& key, V const& val) {
            map[key] = val;
        }

        make_map<K, V>& operator()(K const& key, V const& val) {
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
        make_vector(T const& t) {
            v.push_back(t);
        }

        make_vector& operator()(T const& t) {
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

        ~XPtr() { if (t) delete t; t = NULL; }

        T* operator->() { return t ? t : throw std::runtime_error("pointer is null"); }

        const T* operator->() const { return t ? t : throw std::runtime_error("pointer is null"); }

        operator T*() { return t; }

        operator const T*() const{ return t; }

        template<typename U>
        U* dcast() { return dynamic_cast<U*>(t); }

        template<typename U>
        const U* dcast() const { return dynamic_cast<const U*>(t); }

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

namespace lexer {
    typedef int RuleId;
    typedef int TokenId;
    typedef int Increment;
	typedef int Terminal;

    const RuleId Initial = 0;
    const RuleId InvalidRule = ~0;
    const TokenId InvalidToken = ~0;
    const TokenId Skip = ~0 - 1;

    struct LexerError : std::runtime_error {
        explicit LexerError(const std::string& __arg) : runtime_error(__arg) {}
    };

    struct Transition {
        RuleId ruleId;
        TokenId tokenId;
        bool putChar;
        Increment increment;
        bool lookInSymbolTable;

        explicit Transition(RuleId ruleId = InvalidRule, TokenId tokenId = InvalidToken, bool putChar = true, Increment increment = 1) :
                ruleId(ruleId), tokenId(tokenId), putChar(putChar), increment(increment){}

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

        explicit Condition(std::string const& chars = "", bool inSet = true) : chars(chars), inSet(inSet) {}

        friend bool operator<(Condition const& lhs, Condition const& rhs) {
            if (lhs.chars < rhs.chars)
                return true;
            if (rhs.chars < lhs.chars)
                return false;
            return lhs.inSet < rhs.inSet;
        }
    };

	typedef std::map<Condition, Transition> LexerRule;
	typedef std::map<RuleId, LexerRule> LexerRules;

	typedef std::vector<std::vector<Transition> > LexerStateMachine;

	typedef std::map<std::string, Terminal> SymbolTable;
	typedef common::make_map<std::string, Terminal> MakeSymbolTable;

	typedef std::map<Terminal, std::string> TerminalNames;
	typedef common::make_map<Terminal, std::string> MakeTerminalNames;

	struct Token {
        explicit Token(TokenId tokenId = InvalidToken, std::string const& value = std::string(),
              int startLine = -1, int endLine = -1, int startSymbol = -1, int endSymbol = -1)
                : tokenId(tokenId), value(value),
                  startLine(startLine), endLine(endLine), startSymbol(startSymbol), endSymbol(endSymbol) {}

		std::ostream& stringify(std::ostream& out, TerminalNames const& terminalNames, int maxTerminalNameLength) const {
			TerminalNames::const_iterator terminalNameIt = terminalNames.find(tokenId);
			return out << "("
					  << std::setw(2) << startLine
					  << ", " << std::setw(2) << endLine
					  << ", " << std::setw(2) << startSymbol
					  << ", " << std::setw(2) << endSymbol
					  << "), "
					  << "token: " << std::setw(maxTerminalNameLength)
					  << (terminalNameIt == terminalNames.end() ? common::xsnprintf(32, "%d", tokenId) : terminalNameIt->second) << ", "
					  << (value.empty() ? "" : " value: " + value)
					;
		}

        friend std::ostream& operator<<(std::ostream& os, Token const& token);

        TokenId tokenId;
        std::string value;
        int startLine, endLine, startSymbol, endSymbol;
    };

    typedef std::vector<Token> Tokens;

    class Lexer {
    public:
        explicit Lexer(LexerRules const &lexerRules) {
            lexerStateMachine.resize(lexerRules.size());

            size_t maxChar = std::numeric_limits<uint8_t>::max();

            for_each_c (LexerRules, lexerRules, rule) {
                std::vector<Transition> lexerState;

                lexerState.resize(static_cast<size_t>(maxChar));

                for_each_c (LexerRule, rule->second, transition) {
                    if (transition->first.inSet)
                        for_each_c (std::string, transition->first.chars, c)
                            updateTransition(lexerState, transition->second, *c);
                    else
                        for (char c = 0; c < maxChar; ++c)
                            if (std::find(transition->first.chars.begin(),
                                          transition->first.chars.end(), c) == transition->first.chars.end())
                                updateTransition(lexerState, transition->second, c);
                }

                lexerStateMachine.at(static_cast<size_t>(rule->first)) = lexerState;
            }
        }

        Tokens analize(std::string const& string) {
            RuleId currentState = Initial;
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
                Transition transition = lexerStateMachine.
                        at(static_cast<size_t>(currentState)).
                        at(static_cast<size_t>(*c));

                if (transition.putChar)
                    token.push_back(*c);

                if (transition.ruleId == InvalidRule)
                    throw LexerError(common::xsnprintf(64, "invalid char \"%c\" at %d:%d", *c, line, column));
                else {
                    if (transition.ruleId == Initial) {
                        if (transition.tokenId != Skip)
                            tokens.push_back(Token(static_cast<TokenId>(transition.tokenId), token, prevLine, line, prevColumn, column));
                        prevLine = line;
                        prevColumn = column;
                        token.clear();
                    }
                    currentState = transition.ruleId;
                }
                c += transition.increment;
            }
            return tokens;
        }

        friend std::ostream& operator<<(std::ostream& os, Lexer const& lexer) {
            os << "LexerStateMachine: \n";
            for_each_c (LexerStateMachine, lexer.lexerStateMachine, lexerState) {
                if (lexerState != lexer.lexerStateMachine.begin()) os << "\n";
                for_each_c (std::vector<Transition>, (*lexerState), transition) {
                    if (transition != (*lexerState).begin()) os << ", ";
                    os << *transition;
                }
            }
            os << "\n";
            return os;
        }

    private:
        void updateTransition(std::vector<Transition>& lexerState, Transition const& transition, char symbol) {
            Transition &state = lexerState.at(static_cast<size_t>(symbol));
            if (state != Transition())
                throw LexerError("lexer rule is ambiguous");
            state = transition;
        }

    private:
        LexerStateMachine lexerStateMachine;
    };
}

namespace parser {
    typedef int NonTerminalId;

    typedef std::vector<int> Items;                  // X, Y, Z
    typedef std::vector<Items> Variants;             // alpha | beta | gamma
    typedef std::map<NonTerminalId, Variants> Rules; // A -> alpha | beta | gamma

    typedef common::make_map<NonTerminalId, Variants> MakeRules;
    typedef common::make_vector<Items> MakeItems;
    typedef common::make_vector<int> MakeVariant;

    enum Action {
        Shift,
        Reduce,
        Accept,
        Error,
    };

    typedef int State;

	typedef std::vector<State> StateStack;

    const State StartState = 0;
    const NonTerminalId StartNonTerminal = 0;
    const NonTerminalId Invalid = 0;

    struct ParserError : std::runtime_error {
        explicit ParserError(const std::string& __arg) : runtime_error(__arg) {}
    };

    typedef int ReduceCount;
    struct ActionState {
        Action action;
        State state;
        ReduceCount reduceCount;

        explicit ActionState(Action action = Error, State state = StartState, ReduceCount reduceCount = 0) :
                action(action), state(state), reduceCount(reduceCount){}
    };
    typedef std::vector<std::vector<ActionState> > States;

    class Node;

    typedef std::vector<Node> Nodes;

    struct Production {
        NonTerminalId nonTerminal; // Header
        Nodes nodes; // Body

        explicit Production(NonTerminalId nonTerminal = Invalid, Nodes const& nodes = Nodes()) :
                nonTerminal(nonTerminal), nodes(nodes) {}
    };

    typedef lexer::Token Token;
    typedef lexer::Tokens Tokens;

    typedef std::map<NonTerminalId, std::string> NonTerminalNames;

    class Node {
    public:
        explicit Node(Production const& production) : isProduction_(true), production(production) {}

        explicit Node(Token const& token) : isProduction_(false), token(token) {}

        bool isProduction() const {
            return isProduction_;
        }

        Production const& getProduction() const {
            return production;
        }

        Token const& getToken() const {
            return token;
        }

        static std::string indent2string(int indent = 0) {
            std::stringstream stream;
            while (indent--)
                stream << "    ";
            return stream.str();
        }

        static std::string to_string(int i) {
            std::stringstream stream;
            stream << i;
            return stream.str();
        }

        std::ostream& stringify(std::ostream& out, lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames, int indent = 0) const {
            std::string indentation = indent2string(indent);
            if (isProduction_) {
                NonTerminalNames::const_iterator nonTerminalNameIt = nonTerminalNames.find(production.nonTerminal);
                out << indentation
                       << (nonTerminalNameIt == nonTerminalNames.end() ? to_string(production.nonTerminal)
                                                                       : nonTerminalNameIt->second) << ": {\n";
                for_each_c(Nodes, production.nodes, node) {
                    node->stringify(out, terminalNames, nonTerminalNames, indent + 1);
                }
                out << indentation << "}\n";
            } else {
                lexer::TerminalNames::const_iterator terminalNameIt = terminalNames.find(token.tokenId);
                out << indentation
                       << (terminalNameIt == terminalNames.end() ? to_string(token.tokenId) : terminalNameIt->second)
                       << ": {\n";
                out << indentation << token.value << "\n";
                out << indentation << "}\n";
            }
            return out;
        }

        friend std::ostream& operator<<(std::ostream& os, const Node& node);

    private:
        bool isProduction_;
        Production production;
        Token token;
    };

    class Parser {
    public:
        explicit Parser(Rules const& rules = Rules()) {
			// Пункты



			// Action

			// GoTo
        }

        Node parse(lexer::Tokens const& tokens) {
			StateStack stateStack;
            Nodes stack;
            for_each_c (Tokens, tokens, token) {
                ActionState actionState = action(*token);
                if (actionState.action == Shift) {
                    stack.push_back(Node(*token));
					stateStack.push_back(actionState.state);
                } else if (actionState.action == Reduce) {
                    Nodes nodes;
                    Nodes::iterator start = stack.begin() + (stack.size() - actionState.reduceCount);
                    std::copy(start,
                              stack.end(),
                              std::back_inserter(nodes));
                    nodes.erase(start, stack.end());
                    stack.push_back(Node(Production(actionState.state, nodes)));
					stateStack.push_back(goTo());
                } else if (actionState.action == Error)
                    throw ParserError("error state");
                else if (actionState.action == Accept)
                    return stack.size() == 1 ? stack.back() : throw ParserError("work done but, stack contains more than one element");
            }
            throw ParserError("unexpected end of token string");
        }

    private:
        ActionState action(lexer::Token const& token) {
            return ActionState(Shift, StartState, 0);
        }

        State goTo() {
			return 0;
        }

    private:
        States actionTable;
        int goToTable;
    };
}

namespace json {
    namespace rules {
        enum Terminals {
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
        enum LexerRule {
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
//            IdState,
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
                        (Condition("\""), Transition(StringState, lexer::InvalidToken, false))
                        (Condition("+-0123456789"), Transition(IntegerState))
                        (Condition("."), Transition(FloatState))
                        (Condition("tT"), Transition(BoolState_t))
                        (Condition("fF"), Transition(BoolState_f))
                        (Condition(" "), Transition(Initial, lexer::Skip, false))
                        (Condition("\n"), Transition(Initial, lexer::Skip, false))
                        (Condition("\t"), Transition(Initial, lexer::Skip, false))
//                        (Condition("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"), Transition(IdState))


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
//                (IdState, MakeLexerRule
//                        (Condition("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"), Transition(IdState))
//                        (Condition("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", false), Transition(Initial, lexer::InvalidToken, false, 0, true))
//                )
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

        static lexer::SymbolTable jsonSymbolTable = lexer::MakeSymbolTable
                ("true", Bool)
                ("false", Bool)
        ;

        static lexer::TerminalNames terminalsNames = lexer::MakeTerminalNames
                (ObjectStart, "ObjectStart")
                (ObjectEnd, "ObjectEnd")
                (ArrayStart, "ArrayStart")
                (ArrayEnd, "ArrayEnd")
                (Semicolon, "Semicolon")
                (Comma, "Comma")
                (Integer, "Integer")
                (Float, "Float")
                (String, "String")
                (Bool, "Bool")
        ;

        enum NonTerminals {
            Json,
            Object,
            Array,
            Records,
            Record,
            Values,
            Value,
        };

        typedef parser::Rules Rules;
        typedef parser::MakeRules MakeRules;
        typedef parser::MakeItems MakeItems;
        typedef parser::MakeVariant MakeVariant;

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
        static Rules jsonGrammarRules = MakeRules
                (Json, MakeItems
                        (MakeVariant(Object))
                        (MakeVariant(Array))
                )
                (Object, MakeItems
                        (MakeVariant(ObjectStart)(Records)(ObjectEnd))
                        (MakeVariant(ObjectStart)(ObjectEnd))
                )
                (Array, MakeItems
                        (MakeVariant(ArrayStart)(Values)(ArrayEnd))
                        (MakeVariant(ArrayStart)(ArrayEnd))
                )
                (Records, MakeItems
                        (MakeVariant(Record)(Comma)(Records))
                        (MakeVariant(Record))
                )
                (Record, MakeItems
                        (MakeVariant(String)(Semicolon)(Value))
                )
                (Values, MakeItems
                        (MakeVariant(Value)(Comma)(Values))
                        (MakeVariant(Value))
                )
                (Value, MakeItems
                        (MakeVariant(Object))
                        (MakeVariant(Array))
                        (MakeVariant(String))
                        (MakeVariant(Float))
                        (MakeVariant(Integer))
                        (MakeVariant(Bool))
                )
        ;

        static parser::NonTerminalNames nonTerminalNames = common::make_map<parser::NonTerminalId, std::string>
                (Json,     "Json"    )
                (Object,   "Object"  )
                (Array,    "Array"   )
                (Records,  "Records" )
                (Record,   "Record"  )
                (Values,   "Values"  )
                (Value,    "Value"   )
        ;
    }

    struct JsonError : std::runtime_error {
        JsonError(const std::string& __arg) : runtime_error(__arg) {}
    };

    class Type {
    public:
        Type() {}

        virtual Type* clone() const = 0;

        virtual std::string stringify() const = 0;
    };

    class String : public Type {
    public:
        String(std::string const& value) : value(value){}
        virtual Type* clone() const {
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
        virtual Type* clone() const {
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
        virtual Type* clone() const {
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

        virtual std::string stringify() const {
            std::stringstream stream;
            stream << "{";
            for_each_c (Fields, fields, field) {
                if (field != fields.begin()) stream << ", ";
                stream << "\"" << field->first << "\"" << ": " << field->second->stringify();
            }
            stream << "}";
            return stream.str();
        }

        Object& operator()(std::string const& key, std::string const& v) { return add<String>(key, v); }
        Object& operator()(std::string const& key, const char* v) { return add<String>(key, v); }
        Object& operator()(std::string const& key, int64_t v) { return add<Integer>(key, v); }
        Object& operator()(std::string const& key, int v) { return add<Integer>(key, static_cast<int64_t>(v)); }
        Object& operator()(std::string const& key, bool v) { return add<Bool>(key, v); }
        Object& operator()(std::string const& key, double v) { return add<Float>(key, v); }
        Object& operator()(std::string const& key, Object const& v) { return add<Object>(key, v); }
        Object& operator()(std::string const& key, Array const& v);

        int64_t integerOr(std::string const& key, int64_t defaultValue = 0) {
            return valueOr<Integer>(key, defaultValue);
        }

        std::string stringOr(std::string const& key, std::string const& defaultValue = "") {
            return valueOr<String>(key, defaultValue);
        }

        double floatOr(std::string const& key, double defaultValue = 0.0) {
            return valueOr<Float>(key, defaultValue);
        }

        double boolOr(std::string const& key, bool defaultValue = false) {
            return valueOr<Bool>(key, defaultValue);
        }

        friend std::ostream& operator<<(std::ostream& os, Object const& object) {
            return os << object.stringify();
        }

    private:
        template<typename T, typename V>
        Object& add(std::string const& key, V const& v) {
            json_assert(fields.insert(std::make_pair(key, new T(v))).second, "can't insert field");
            return *this;
        };

        template <typename T>
        const T* get(std::string const& key) const {
            Fields::iterator it = fields.find(key);
            return it == fields.end() ? NULL : it->second.dcast<T>();
        }

        template<typename T, typename V>
        V valueOr(std::string const& key, V const& defaultValue = 0) {
            const T* value = get<T>(key);
            return value ? value->value : defaultValue;
        }

        mutable Fields fields;
    };

    class Array : public Type {
    public:
        typedef std::vector<common::XPtr<Type>, common::clean_allocator<common::XPtr<Type> > > Fields;
        Array() {}

        virtual Array* clone() const {
            return new Array(*this);
        }

        Array& operator()(std::string const& v) { return add<String>(v); }
        Array& operator()(const char* v) { return add<String>(v); }
        Array& operator()(int64_t v) { return add<Integer>(v); }
        Array& operator()(int v) { return add<Integer>(static_cast<int64_t>(v)); }
        Array& operator()(bool v) { return add<Bool>(v); }
        Array& operator()(double v) { return add<Float>(v); }
        Array& operator()(Object const& v) { return add<Object>(v); }
        Array& operator()(Array const& v) { return add<Array>(v); }

        int64_t integerOr(size_t index, int64_t defaultValue = 0) {
            return valueOr<Integer>(index, defaultValue);
        }

        std::string stringOr(size_t index, std::string const defaultValue = "") {
            return valueOr<String>(index, defaultValue);
        }

        double floatOr(size_t index, double defaultValue = 0.0) {
            return valueOr<Float>(index, defaultValue);
        }

        bool boolOr(size_t index, bool defaultValue = false) {
            return valueOr<Bool>(index, defaultValue);
        }

        virtual std::string stringify() const {
            std::stringstream stream;
            stream << "[";
            for_each_c (Fields, fields, field) {
                if (field != fields.begin()) stream << ", ";
                stream << (*field)->stringify();
            }
            stream << "]";
            std::string string = stream.str();
            return string;
        }

        friend std::ostream& operator<<(std::ostream& os, Array const& array) {
            return os << array.stringify();
        }

    private:
        template<typename T, typename V>
        Array& add(V const& v) {
            fields.push_back(new T(v));
            return *this;
        };

        template <typename T>
        const T* get(size_t index) {
            return fields.at(index).dcast<T>();
        }

        template <typename T, typename V>
        V valueOr(size_t index, V const& defaultValue) {
            const T* value = get<T>(index);
            return value ? value->value : defaultValue;
        };

        Fields fields;
    };

    Object& Object::operator()(std::string const& key, Array const& v) {
        return add<Array>(key, v);
    }

    lexer::Lexer jsonLexer(rules::lexerRules);
    parser::Parser jsonParser(rules::jsonGrammarRules);

    class Json {
        enum Type {
            IsInvalid,
            IsObject,
            IsArray,
        };
    public:
        Json() : type(IsInvalid) {}

        explicit Json(parser::Node const& node) : type(IsInvalid) {
            init(node);
        }

        explicit Json(std::string const& string) : type(IsInvalid) {
            init(string);
        }

        template <typename Iterator>
        Json(Iterator begin, Iterator end) : type(IsInvalid) {
            init(std::string(begin, end));
        }

        explicit Json(std::istream& istream) : type(IsInvalid) {
            init(std::string(std::istreambuf_iterator<char>(istream), std::istreambuf_iterator<char>()));
        }

        bool hasObject() const {
            return type == IsObject;
        }

        bool hasArray() const {
            return type == IsArray;
        }

        Object const& getObject() const {
            if (hasObject())
                return object;
            throw JsonError("json root is array");
        }

        Array const& getArray() const {
            if (hasArray())
                return array;
            throw JsonError("json root is object");
        }

        friend std::ostream& operator<<(std::ostream& os, const Json& json) {
            return json.hasObject() ? (os << json.object) : json.hasArray() ? (os << json.array) : os;
        }

    private:
        static Object createObject(parser::Node const& root) {
            Object object;
//			for_each_c(parser::Nodes, root.getProduction().nodes, node) {
//				if (node->isProduction()) {
//					if (node->getProduction().nonTerminal == rules::String)
//				}
//			}
            return object;
        }

        static Array createArray(parser::Node const& node) {
            Array array;
            return array;
        }

        void init(parser::Node const& node) {
            if (node.isProduction())
                if (rules::Object == node.getProduction().nonTerminal)
                    object = createObject(node);
                else if (rules::Array == node.getProduction().nonTerminal)
                    array = createArray(node);
                else
                    throw JsonError("invalid format");
            else
                throw JsonError("invalid format");
        }

        void init(std::string const& string) {
            lexer::Tokens tokens = jsonLexer.analize(string);

            for_each (lexer::Tokens, tokens, token) {
                std::cout << *token << std::endl;
            }

            parser::Node ast = jsonParser.parse(tokens);

            std::cout << ast << std::endl;

            init(ast);
        }

        Type type;
        Object object;
        Array array;
    };
}
namespace lexer {
	std::ostream& operator<<(std::ostream& os, lexer::Token const& token) {
		int maxNameLength = 0;
		for_each_c(lexer::TerminalNames, json::rules::terminalsNames, terminalName)
			maxNameLength = std::max<int>(maxNameLength, terminalName->second.size());
		return token.stringify(os, json::rules::terminalsNames, maxNameLength);
	}
}
namespace parser {
    std::ostream& operator<<(std::ostream& os, const parser::Node& node) {
        return node.stringify(os, json::rules::terminalsNames, json::rules::nonTerminalNames);
    }
}

int main() {
    try {
        std::ifstream file("../test.json");
        json::Json js(file);
        json::Json json1;
        std::cout << js << std::endl;

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