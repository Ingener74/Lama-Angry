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
#include <iterator>

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

    std::string terminalName(Terminal terminal, TerminalNames const& terminalNames) {
        TerminalNames::const_iterator it = terminalNames.find(terminal);
        if (it == terminalNames.end()) {
            std::stringstream stringstream;
            stringstream << terminal;
            return stringstream.str();
        }
        return it->second;
    }

    struct Token {
        explicit Token(TokenId tokenId = InvalidToken, std::string const& value = std::string(),
              int startLine = -1, int endLine = -1, int startSymbol = -1, int endSymbol = -1)
                : tokenId(tokenId), value(value),
                  startLine(startLine), endLine(endLine), startSymbol(startSymbol), endSymbol(endSymbol) {}

        std::ostream& stringify(std::ostream& out, TerminalNames const& terminalNames, int maxTerminalNameLength) const {
            return out << "("
                       << std::setw(2) << startLine
                       << ", " << std::setw(2) << endLine
                       << ", " << std::setw(2) << startSymbol
                       << ", " << std::setw(2) << endSymbol
                       << "), "
                       << "token: " << std::setw(maxTerminalNameLength)
                       << terminalName(tokenId, terminalNames) << ", "
                       << (value.empty() ? "" : " value: " + value);
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
    typedef int NonTerminal;

    typedef std::map<NonTerminal, std::string> NonTerminalNames;

    std::string nonTerminalName(NonTerminal nonTerminal, NonTerminalNames const& nonTerminalNames) {
        NonTerminalNames::const_iterator it = nonTerminalNames.find(nonTerminal);
        if (it == nonTerminalNames.end()) {
            std::stringstream stringstream;
            stringstream << nonTerminal;
            return stringstream.str();
        }
        return it->second;
    }

    struct GrammaticSymbol {
        int value; // Terminal or NonTerminal
        bool isTerminal;

        explicit GrammaticSymbol(int value, bool isTerminal = true) : value(value), isTerminal(isTerminal) {}

        friend bool operator==(const GrammaticSymbol& lhs, const GrammaticSymbol& rhs) {
            return lhs.value == rhs.value &&
                   lhs.isTerminal == rhs.isTerminal;
        }

        friend bool operator!=(const GrammaticSymbol& lhs, const GrammaticSymbol& rhs) {
            return !(rhs == lhs);
        }

        friend bool operator<(const GrammaticSymbol& lhs, const GrammaticSymbol& rhs) {
            if (lhs.value < rhs.value)
                return true;
            if (rhs.value < lhs.value)
                return false;
            return lhs.isTerminal < rhs.isTerminal;
        }

        std::string stringify(lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames) const {
            std::stringstream stringstream;
            stringstream << std::setw(14)
                         << (isTerminal ? "Terminal" : "NonTerminal") << ": "
                         << (isTerminal ? lexer::terminalName(value, terminalNames) : nonTerminalName(value, nonTerminalNames));
            return stringstream.str();
        }
    };

    typedef std::vector<GrammaticSymbol> Items;             // X, Y, Z
    typedef std::vector<Items> Variants;             // alpha | beta | gamma
    typedef std::map<NonTerminal, Variants> Rules;   // A -> alpha | beta | gamma

    typedef common::make_map<NonTerminal, Variants> MakeRules;
    typedef common::make_vector<Items> MakeItems;
    typedef common::make_vector<GrammaticSymbol> MakeVariant;


//    struct GrammarBuilder;
//
//    struct ProductionBuilder : GrammarBuilder {
//
//    };
//
//    struct GrammarBuilder {
//        Rules rules;
//
//        GrammarBuilder& addProduction(NonTerminal nonTerminal, Items const& items) {
//            rules.insert(std::make_pair(nonTerminal, items));
//            return *this;
//        }
//
//        operator Rules() const {
//            return rules;
//        }
//
//
//    };

    enum Action {
        Shift,
        Reduce,
        Accept,
        Error,
    };

    typedef int State;

	typedef std::vector<State> StateStack;
    typedef std::vector<State> StateTable;

    const State StartState = 0;
    const NonTerminal StartNonTerminal = 0;
    const NonTerminal InitialNonTerminal = 1;
    const NonTerminal Invalid = 0;

    struct Convolution {
        NonTerminal header;
        Items body;

        explicit Convolution(NonTerminal header = Invalid, Items const& body = Items()) : header(header), body(body)
        {}

        virtual std::string stringify(lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames) {
            std::stringstream stringstream;
            stringstream << nonTerminalName(header, nonTerminalNames) << " -> ";
            for_each(Items, body, item) {
                if (item != body.begin()) stringstream << ", ";
                stringstream << item->stringify(terminalNames, nonTerminalNames);
            }
            return stringstream.str();
        }

        friend bool operator==(const Convolution& lhs, const Convolution& rhs) {
            return lhs.header == rhs.header &&
                   lhs.body == rhs.body;
        }
    };

    struct ParserError : std::runtime_error {
        explicit ParserError(const std::string& __arg) : runtime_error(__arg) {}
    };

	struct ActionState {
        Action action;
        State state;
		NonTerminal nonTerminal;
        size_t reduceCount;

        explicit ActionState(Action action = Error, State state = StartState, NonTerminal nonTerminal = Invalid, size_t reduceCount = 0) :
                action(action), state(state), nonTerminal(nonTerminal), reduceCount(reduceCount)
        {}

		friend bool operator==(const ActionState& lhs, const ActionState& rhs)
		{
			return lhs.action == rhs.action &&
				   lhs.state == rhs.state &&
				   lhs.nonTerminal == rhs.nonTerminal &&
                   lhs.reduceCount == rhs.reduceCount;
		}

		friend bool operator!=(const ActionState& lhs, const ActionState& rhs)
		{
			return !(rhs == lhs);
		}

        std::ostream& stringify(std::ostream& out, NonTerminalNames const& nonTerminalNames) const {
            out << "[" << (action == Shift ? "S" : action == Reduce ? "R" : action == Accept ? "A" : action == Error ? "E" : throw ParserError("invalid action"))
                << ", " << state;
            if (nonTerminal == Invalid)
                return out << "]";
            return out << ", "
                       << nonTerminalName(nonTerminal, nonTerminalNames)
                       << reduceCount  << "]";
        }
        std::string stringify(NonTerminalNames const& nonTerminalNames) const {
            std::stringstream s;
            stringify(s, nonTerminalNames);
            return s.str();
        }
    };
    typedef std::vector<std::vector<ActionState> > States;

    class Node;

    typedef std::vector<Node> Nodes;

    struct Production {
        NonTerminal nonTerminal; // Header
        Nodes nodes; // Body

        explicit Production(NonTerminal nonTerminal = Invalid, Nodes const& nodes = Nodes()) :
                nonTerminal(nonTerminal), nodes(nodes) {}
    };

    typedef lexer::Token Token;
    typedef lexer::Tokens Tokens;

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
                stream << "  ";
            return stream.str();
        }

        static std::string to_string(int i) {
            std::stringstream stream;
            stream << i;
            return stream.str();
        }

        std::ostream& stringify(std::ostream& out, lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames, int indent) const {
            std::string indentation = indent2string(indent);
            if (isProduction_) {
                out << nonTerminalName(production.nonTerminal, nonTerminalNames) << "+";
                for_each_c(Nodes, production.nodes, node) {
                    out << std::endl;
                    out << indentation;
                    node->stringify(out, terminalNames, nonTerminalNames, indent + 1);
                }
            } else {
                out << lexer::terminalName(token.tokenId, terminalNames) << " - " << token.value;
            }
            return out;
        }

        std::ostream& stringify(std::ostream& out, lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames) const {
            return out << (isProduction_ ? nonTerminalName(production.nonTerminal, nonTerminalNames)
                                         : lexer::terminalName(token.tokenId, terminalNames));
        }

        friend std::ostream& operator<<(std::ostream& os, const Node& node);

    private:
        bool isProduction_;
        Production production;
        Token token;
    };

    struct Situation : Convolution {
        size_t point;

        explicit Situation(NonTerminal nonTerminal = Invalid, Items const& items = Items()) : Convolution(nonTerminal, items), point(0) {
        }

        virtual std::string stringify(lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames) const {
            std::stringstream stringstream;
            stringstream << nonTerminalName(header, nonTerminalNames) << " -> ";
            for_each_c(Items, body, item) {
                if (std::distance(body.begin(), item) == point) stringstream << "^";
                stringstream << " "
                             << (item->isTerminal ? lexer::terminalName(item->value, terminalNames) : nonTerminalName(item->value, nonTerminalNames))
                             << " ";
            }
            if (body.size() == point)
                stringstream << "^";
            return stringstream.str();
        }

        friend bool operator==(const Situation& lhs, const Situation& rhs) {
            return static_cast<const Convolution&>(lhs) == static_cast<const Convolution&>(rhs) &&
                   lhs.point == rhs.point;
        }
    };

    typedef std::vector<Situation> Situations;

    typedef std::set<GrammaticSymbol> UniqueGrammaticSymbols;

    struct CanonicalItem {
        GrammaticSymbol terminal;
        Situations situations;

        CanonicalItem(GrammaticSymbol const& terminal, Situations const& situations) : terminal(terminal),
                                                                                       situations(situations) {}
        std::string stringify(lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames) const {
            std::stringstream stringstream;
            stringstream << terminal.stringify(terminalNames, nonTerminalNames) << "\n";
            for_each_c(Situations, situations, s)
                stringstream << "\t\t" << s->stringify(terminalNames, nonTerminalNames) << "\n";
            return stringstream.str();
        }
    };

    typedef std::vector<CanonicalItem> CanonicalSet;

    struct Pred {
        Situations const& s;
        explicit Pred(const Situations& s) : s(s) {}
        bool operator()(CanonicalItem const& rhs) const { return s == rhs.situations; }
    };

    typedef std::set<lexer::Terminal> UniqueTerminals;

    typedef std::set<NonTerminal> UniqueNonTerminals;

    class Parser {
    public:
        explicit Parser(Rules const& rules = Rules(),
                        lexer::TerminalNames const& terminalNames = lexer::TerminalNames(),
                        NonTerminalNames const& nonTerminalNames = NonTerminalNames()) : states(0), terminals(0),
                                                                                         nonTerminals(0) {
            UniqueGrammaticSymbols uniqueGrammaticSymbols;
            for_each_c(Rules, rules, rule) {
                uniqueGrammaticSymbols.insert(GrammaticSymbol(rule->first, false));
                for_each_c(Variants, rule->second, variants) {
                    for_each_c(Items, (*variants), item) {
                        uniqueGrammaticSymbols.insert(*item);
                    }
                }
            }

            CanonicalSet canonicalSet = items(rules, uniqueGrammaticSymbols);

            UniqueTerminals uniqueTerminals;
            for_each(UniqueGrammaticSymbols, uniqueGrammaticSymbols, symbol)
                if (symbol->isTerminal)
                    uniqueTerminals.insert(symbol->value);
            UniqueNonTerminals uniqueNonTerminals;
            for_each(UniqueGrammaticSymbols, uniqueGrammaticSymbols, symbol)
                if (!symbol->isTerminal)
                    uniqueNonTerminals.insert(symbol->value);

//            std::cout << canonicalSet.size() << std::endl;
//            for_each(CanonicalSet, canonicalSet, item)
//                std::cout << item->stringify(terminalNames, nonTerminalNames) << std::endl;

//            std::ofstream dotFile("canonical_set.dot");
//            dotFile << dotStringify(rules, canonicalSet, terminalNames, nonTerminalNames);

            states = canonicalSet.size();
            terminals = uniqueTerminals.size();
            nonTerminals = uniqueNonTerminals.size();

            actionTable.resize(states * terminals);
            goToTable.resize(states * nonTerminals);
            for_each(StateTable, goToTable, gt) *gt = -1;

            for (size_t i = 0; i < canonicalSet.size(); ++i) {
                for_each_c(Situations, canonicalSet.at(i).situations, situation) {
                    for_each(UniqueTerminals, uniqueTerminals, terminal) {
                        if (situation->body.size() == situation->point) {
                            getAction(i, *terminal).push_back(
                                    situation->header == StartNonTerminal ? ActionState(Accept, 0, 0, 0)
                                                                          : ActionState(Reduce, i, situation->header, situation->body.size()));
                        } else {
                            if (!situation->body.at(situation->point).isTerminal)
                                continue;

                            Situations goToSits = goTo(rules, canonicalSet.at(i).situations, GrammaticSymbol(*terminal));

                            for (size_t j = 0; j < canonicalSet.size(); ++j) {
                                if (canonicalSet.at(j).situations == goToSits) {
                                    ActionState actionState(Shift, j, 0, 0);
                                    std::vector<ActionState>& actions = getAction(i, *terminal);
                                    if (std::find(actions.begin(), actions.end(), actionState) == actions.end()) {
                                        actions.push_back(actionState);
                                    }
                                }
                            }
                        }
                    }
                }
                for_each(UniqueNonTerminals, uniqueNonTerminals, nonTerminal) {
                    Situations gts = goTo(rules, canonicalSet.at(i).situations, GrammaticSymbol(*nonTerminal, false));
                    for (State j = 0; j < canonicalSet.size(); ++j) {
                        if (gts == canonicalSet.at(j).situations) {
                            getGoTo(i, *nonTerminal) = j;
                        }
                    }
                }
            }
        }

        std::string dotStringify(Rules const& grammar,
                                 CanonicalSet const& canonicalSet,
                                 lexer::TerminalNames const& terminalNames,
                                 NonTerminalNames const& nonTerminalNames) {
            std::stringstream s;
            s << "digraph CanonicalSet {" << std::endl;

            for (size_t i = 0; i < canonicalSet.size(); ++i) {
                s << "I" << i << "[label=\"";
                for_each_c(Situations, canonicalSet.at(i).situations, sit) {
                    if (sit != canonicalSet.at(i).situations.begin()) s  << "\n";
                    s << sit->stringify(terminalNames, nonTerminalNames);
                }
                s << "\"]\n";
            }

            for (size_t i = 0; i < canonicalSet.size(); ++i) {
                UniqueGrammaticSymbols uniqueGrammaticSymbols;
                for_each_c(Situations, canonicalSet.at(i).situations, situation) {
                    if (situation->body.size() == situation->point)
                        continue;
                    uniqueGrammaticSymbols.insert(situation->body.at(situation->point));
                }

                for_each(UniqueGrammaticSymbols, uniqueGrammaticSymbols, symbol) {
                    Situations goToSits = goTo(grammar, canonicalSet.at(i).situations, *symbol);

                    for (size_t j = 0; j < canonicalSet.size(); ++j) {
                        if (goToSits == canonicalSet.at(j).situations) {
                            s << "I" << i << " -> " << "I" << j << " [label=\""
                              << (symbol->isTerminal ? lexer::terminalName(symbol->value, terminalNames)
                                                     : nonTerminalName(symbol->value, nonTerminalNames)) << "\"]" << std::endl;
                        }
                    }
                }
            }
            s << "}";
            return s.str();
        }

        static CanonicalSet items(Rules const& rules, UniqueGrammaticSymbols const& uniqueGrammaticSymbols) {
            CanonicalSet canonicalSet;

            Situations situations;
            Situation start(StartNonTerminal, MakeVariant(GrammaticSymbol(InitialNonTerminal, false)));
            situations.push_back(start);
            closure(situations, rules, start);

            canonicalSet.push_back(CanonicalItem(GrammaticSymbol(lexer::InvalidToken), situations));

            int itemAdded;
            do {
                itemAdded = 0;
                CanonicalSet t;
                for_each(CanonicalSet, canonicalSet, canonicalItem) {
                    for_each(UniqueGrammaticSymbols, uniqueGrammaticSymbols, symbol) {
                        Situations goToSituation = goTo(rules, canonicalItem->situations, *symbol/*, terminalNames, nonTerminalNames*/);
                        if (goToSituation.empty())
                            continue;
                        if (std::find_if(canonicalSet.begin(), canonicalSet.end(), Pred(goToSituation)) == canonicalSet.end() &&
                            std::find_if(t.begin(), t.end(), Pred(goToSituation)) == t.end()) {
                            t.push_back(CanonicalItem(*symbol, goToSituation));
                            itemAdded++;
                        }
                    }
                }
                std::copy(t.begin(), t.end(), std::back_inserter(canonicalSet));
            } while (itemAdded > 0);

            return canonicalSet;
        }

        static void closure(Situations& situations, Rules const& grammar, Situation const& situation) {
            if (situation.body.size() == situation.point)
                return;
            if (situation.body.at(situation.point).isTerminal)
                return;
            Rules::const_iterator rule = grammar.find(situation.body.at(situation.point).value);
            for_each_c(Variants, rule->second, v) {
                Situation s(rule->first, *v);
                if (std::find(situations.begin(), situations.end(), s) == situations.end()) {
                    situations.push_back(s);
                    closure(situations, grammar, s);
                }
            }
        }

        static Situations goTo(Rules const& grammar, Situations const& situations, GrammaticSymbol const& grammaticSymbol/*,
                               lexer::TerminalNames const& terminalNames = lexer::TerminalNames(),
                               NonTerminalNames const& nonTerminalNames = NonTerminalNames()*/) {
            Situations goToSituations;
            for_each_c(Situations, situations, s) {
                if (s->body.size() == s->point)
                    continue;
                if (s->body.at(s->point) == grammaticSymbol) {
                    goToSituations.push_back(*s);
                }
            }
            if (goToSituations.empty()) {
                return Situations();
            }
            Situations goToClosure;
            for_each(Situations, goToSituations, goToSit) {
                goToSit->point++;
                closure(goToClosure, grammar, *goToSit);
            }
            std::copy(goToSituations.begin(), goToSituations.end(), std::inserter(goToClosure, goToClosure.begin()));
            return goToClosure;
        }

        Node parse(lexer::Tokens const& tokens,
                   lexer::TerminalNames const& terminalNames = lexer::TerminalNames(),
                   NonTerminalNames const& nonTerminalNames = NonTerminalNames()) {
            StateStack stateStack;
            Nodes nodeStack;
            stateStack.push_back(StartNonTerminal);
            for (Tokens::const_iterator token = tokens.begin(); /*token != tokens.end()*/; /*++token*/) {
                ActionState actionState = action(stateStack.back(), token->tokenId);
                if (actionState.action == Shift) {
                    nodeStack.push_back(Node(*token));
                    stateStack.push_back(actionState.state);
                    
                    stringifyStep(std::cout, stateStack, nodeStack, actionState, *token, terminalNames, nonTerminalNames);
                    std::cout << std::endl;
                    
                    ++token;
                } else if (actionState.action == Reduce) {
                    Nodes nodes;
                    Nodes::iterator start = nodeStack.end() - actionState.reduceCount;
                    std::copy(start,
                              nodeStack.end(),
                              std::back_inserter(nodes));

                    nodeStack.erase(start, nodeStack.end());
                    nodeStack.push_back(Node(Production(actionState.nonTerminal, nodes)));

                    for (size_t i = 0; i < actionState.reduceCount; ++i)
                        stateStack.pop_back();
                    stateStack.push_back(goTo(stateStack.back(), actionState.nonTerminal));

                    stringifyStep(std::cout, stateStack, nodeStack, actionState, *token, terminalNames, nonTerminalNames);
                    std::cout << std::endl;
                } else if (actionState.action == Error) {
                    throw ParserError("error state");
                } else if (actionState.action == Accept) {
                    return nodeStack.size() == 1
                           ? nodeStack.back()
                           : throw ParserError("work done but, stack contains more than one element");
                }
            }
        }

        std::ostream& stringifyStep(std::ostream& out, StateStack const& stateStack, Nodes const& nodes, ActionState const& actionState,
                                  Token const& nextToken,
                                  lexer::TerminalNames const& terminalNames = lexer::TerminalNames(),
                                  NonTerminalNames const& nonTerminalNames = NonTerminalNames()) {
            const int w = 120;
            {
                std::stringstream s;
                for_each_c(StateStack, stateStack, state) {
                    if (state != stateStack.begin()) s << " ";
                    s << *state;
                }
                out << std::setw(w) << std::left << s.str();
            }
            out << "; ";

            {
                std::stringstream s;
                for_each_c(Nodes, nodes, node) {
                    if (node != nodes.begin()) s << ", ";
                    node->stringify(s, terminalNames, nonTerminalNames);
                }
                out << std::setw(w) << std::left << s.str();
            }
            out << "; ";

            {
                std::stringstream s;
                nextToken.stringify(s, terminalNames, 0);
                out << std::setw(w) << std::left << s.str();
            }
            out << "; ";

            {
                std::stringstream s;
                actionState.stringify(s, nonTerminalNames);
                out << std::setw(w) << std::left << s.str();
            }
            return out;
        }

        std::string stringify(lexer::TerminalNames const& terminalNames, NonTerminalNames const& nonTerminalNames) {

            size_t extStates = states + 1;
            size_t extTerminals = terminals + 1;

            std::vector<std::string> actionsCells(extStates * extTerminals);
            std::vector<std::string> goToCells(extStates * nonTerminals);
            std::vector<size_t> actionWidths(extTerminals);
            std::vector<size_t> gotoWidths(nonTerminals);

            for (int i = 0; i < states; ++i) {
                std::stringstream s;
                s << i;
                std::string& str = actionsCells.at((i + 1) * extTerminals);
                str = s.str();
                actionWidths.at(0) = std::max(actionWidths.at(0), str.size());
            }
            for (size_t i = 0; i < terminals; ++i) {
                std::string str = lexer::terminalName(i, terminalNames);
                actionsCells.at(i + 1) = str;
                actionWidths.at(i + 1) = std::max(actionWidths.at(i + 1), str.size());
            }
            for (size_t i = 0; i < nonTerminals; ++i) {
                std::string str = nonTerminalName(i, nonTerminalNames);
                goToCells.at(i) = str;
                gotoWidths.at(i) = std::max(gotoWidths.at(i), str.size());
            }
            for (size_t i = 0; i < states; ++i) {
                for (size_t j = 0; j < terminals; ++j) {
                    std::stringstream s;
                    for (size_t n = 0; n < getAction(i, j).size(); ++n) {
                        if (n > 0) s << ", ";
                        const std::string& string = getAction(i, j).at(n).stringify(nonTerminalNames);
                        s << string;
                    }
                    std::string& str = actionsCells.at((i + 1) * extTerminals + (j + 1));
                    str = s.str();
                    actionWidths.at(j + 1) = std::max(actionWidths.at(j + 1), str.size());
                }
            }

            for (size_t i = 0; i < states; ++i) {
                for (size_t j = 0; j < nonTerminals; ++j) {
                    std::stringstream s;
                    s << getGoTo(i, j);
                    std::string& str = goToCells.at((i + 1) * nonTerminals + j);
                    str = s.str();
                    gotoWidths.at(j) = std::max(gotoWidths.at(j), str.size());
                }
            }

            std::stringstream s;

            for (size_t i = 0; i < extStates; ++i) {
                if (i > 0) s << "\n";
                for (size_t j = 0; j < extTerminals; ++j) {
                    if (j > 0) s << ", ";
                    s << "[";
                    std::string& str = actionsCells.at(i * extTerminals + j);
                    s << str;
                    size_t w = str.size();
                    while (w++ < actionWidths.at(j)) s << " ";
                    s << "]";
                }
                for (size_t j = 0; j < nonTerminals; ++j) {
                    s << ", ";
                    std::string& str = goToCells.at(i * nonTerminals + j);
                    s << str;
                    size_t w = str.size();
                    while (w++ < gotoWidths.at(j)) s << " ";
                }
            }
            return s.str();
        }

    private:
        ActionState action(State curState, lexer::Terminal terminal) {
            std::vector<ActionState> const& act = getAction(curState, terminal);
            return act.empty() ? ActionState(Shift, 0, 0, 0) : act.at(0);
        }

        State goTo(State curState, NonTerminal nonTerminal) {
            return goToTable.at(curState * nonTerminals + nonTerminal);
        }

        std::vector<ActionState>& getAction(State state, lexer::Terminal terminal) {
            if (actionTable.empty())
                throw ParserError("action is empty");
            return actionTable.at(state * terminals + terminal);
        }

        State& getGoTo(State state, NonTerminal nonTerminal) {
            if (goToTable.empty())
                throw ParserError("go to is empty");
            return goToTable.at(state * nonTerminals + nonTerminal);
        }

        States actionTable;
        StateTable goToTable;
        size_t states;
        size_t terminals;
        size_t nonTerminals;
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

//        static lexer::TerminalNames terminalsNames = lexer::MakeTerminalNames
//                (ObjectStart, "ObjectStart")
//                (ObjectEnd, "ObjectEnd")
//                (ArrayStart, "ArrayStart")
//                (ArrayEnd, "ArrayEnd")
//                (Semicolon, "Semicolon")
//                (Comma, "Comma")
//                (Integer, "Integer")
//                (Float, "Float")
//                (String, "String")
//                (Bool, "Bool")
//        ;
        static lexer::TerminalNames terminalsNames = lexer::MakeTerminalNames
                (ObjectStart, "Os")
                (ObjectEnd, "Oe")
                (ArrayStart, "As")
                (ArrayEnd, "Ae")
                (Semicolon, "s:")
                (Comma, "c,")
                (Integer, "i")
                (Float, "f")
                (String, "s")
                (Bool, "b")
        ;

        enum NonTerminals {
            Json = parser::InitialNonTerminal,
            Object,
            Array,
            Records,
            Record,
            Values,
            Value,
        };

        typedef parser::GrammaticSymbol GrammaticSym;
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

        /*
           GrammarBuilder.
                rule(Json).nonTerm(Object).
                rule(Json).nonTerm(Array).
                rule(Object).term(ObjectStart).nonTerm(Records).term(ObjectEnd).
                rule(Object).term(ObjectStart).term(ObjectEnd).
                rule().
                    ....
         */

        static Rules jsonGrammarRules = MakeRules
                (Json, MakeItems
                        (MakeVariant(GrammaticSym(Object, false)))
                        (MakeVariant(GrammaticSym(Array, false)))
                )
                (Object, MakeItems
                        (MakeVariant(GrammaticSym(ObjectStart))(GrammaticSym(Records, false))(GrammaticSym(ObjectEnd)))
                        (MakeVariant(GrammaticSym(ObjectStart))(GrammaticSym(ObjectEnd)))
                )
                (Array, MakeItems
                        (MakeVariant(GrammaticSym(ArrayStart))(GrammaticSym(Values, false))(GrammaticSym(ArrayEnd)))
                        (MakeVariant(GrammaticSym(ArrayStart))(GrammaticSym(ArrayEnd)))
                )
                (Records, MakeItems
                        (MakeVariant(GrammaticSym(Record, false))(GrammaticSym(Comma))(GrammaticSym(Records, false)))
                        (MakeVariant(GrammaticSym(Record, false)))
                )
                (Record, MakeItems
                        (MakeVariant(GrammaticSym(String))(GrammaticSym(Semicolon))(GrammaticSym(Value, false)))
                )
                (Values, MakeItems
                        (MakeVariant(GrammaticSym(Value, false))(GrammaticSym(Comma))(GrammaticSym(Values, false)))
                        (MakeVariant(GrammaticSym(Value, false)))
                )
                (Value, MakeItems
                        (MakeVariant(GrammaticSym(Object, false)))
                        (MakeVariant(GrammaticSym(Array, false)))
                        (MakeVariant(GrammaticSym(String)))
                        (MakeVariant(GrammaticSym(Float)))
                        (MakeVariant(GrammaticSym(Integer)))
                        (MakeVariant(GrammaticSym(Bool)))
                )
        ;

//        static parser::NonTerminalNames nonTerminalNames = common::make_map<parser::NonTerminal, std::string>
//                (Json,     "Json"    )
//                (Object,   "Object"  )
//                (Array,    "Array"   )
//                (Records,  "Records" )
//                (Record,   "Record"  )
//                (Values,   "Values"  )
//                (Value,    "Value"   )
//        ;
        static parser::NonTerminalNames nonTerminalNames = common::make_map<parser::NonTerminal, std::string>
                (Json,     "J"    )
                (Object,   "O"  )
                (Array,    "A"   )
                (Records,  "RS" )
                (Record,   "R"  )
                (Values,   "VS"  )
                (Value,    "V"   )
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
    parser::Parser jsonParser(rules::jsonGrammarRules, json::rules::terminalsNames, json::rules::nonTerminalNames);

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

			std::cout << jsonParser.stringify(json::rules::terminalsNames, json::rules::nonTerminalNames) << std::endl;

            parser::Node ast = jsonParser.parse(tokens, json::rules::terminalsNames, json::rules::nonTerminalNames);

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
        return node.stringify(os, json::rules::terminalsNames, json::rules::nonTerminalNames, 0);
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