---
title: Functor, Applicative, and Monad
date: 2016-12-12T12:55:56+09:00
published: true
withtoc: true
tags: Haskell, Functor, Applicative, Monad
---

https://www.schoolofhaskell.com/school/advanced-haskell/functors-applicative-functors-and-monads

# sdklfji
sdlkfjlksdf

## sdklfjwioef
sdfjiowejifowef

### kjowie
sdfkjwioefj

## jiwoefjiowef

### wjoifjwioef
wioefjiowefjiowef

# iow8938

## wioefjoi
sdklfjsoidfjowef

아주 간단한 문제로 시작해봅시다. 우리는 사용자로부터 생일을 입력받기를 원합니다. 그리고, 2020년에 사용자의 나이를 알려주는 프로그램을 작성할 것입니다. read 함수를 사용한 아주 간단한 프로그램입니다.

```haskell
main = do
    putStrLn "Please enter your birth year"
    year <- getLine
    putStrLn $ "In 2020, you will be: " ++ show (2020 - read year)
```
<!--more-->

만약 그 프로그램을 실행 후 유효한 년도를 입력하면, 올바른 답을 구하실 것입니다. 만약 이상한 년도 값을 입력하면 어떤 일이 벌어질까요?

```sh
Please enter your birth year
hello
main.hs: Prelude.read: no parse
```

사용자 입력은 문자로 들어오고 그 문자를 Integer로 변환할려고 합니다. 그러나, 모든 문자들이 항상 유효한 Integer값은 아닙니다. read는 partial 함수입니다. 그것은 어떤 상황에서는 에러를 발생시킬수 있다는 것을 의미합니다.

좀더 견고한 프로그램을 작성하는 방법은 Maybe Integer 값을 반환하는 readMay함수를 쓰는것입니다. 이 함수는 그 파싱이 성공 또는 실패할지도 모르는 형들을 분명히 한다. 이것을 테스트할려면, 다음 코드를 실행해보세요.
A more resilient way to write our code is to use the readMay function, which will return a Maybe Integer value. This makes it clear with the types themselves that the parse may succeed or fail. To test this out, try running the following code:

```haskell
import Safe (readMay)

main = do
    -- We use explicit types to tell the compiler how to try and parse the
    -- string.
    print (readMay "1980" :: Maybe Integer)
    print (readMay "hello" :: Maybe Integer)
    print (readMay "2000" :: Maybe Integer)
    print (readMay "two-thousand" :: Maybe Integer)
```

그래서 어떻게 우리는 이 코드를 우리의 원래 문제를 풀기 위해서 사용할 수 있을까요? 우리는 readMay의 결과값 성공(Just) 또는 실패(Nothing)를 결정할 필요가 있습니다. 그 중 한 방법은 패턴 매칭을 사용하는 것입니다.

```haskell
import Safe (readMay)

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    case readMay yearString of
        Nothing -> putStrLn "You provided an invalid year"
        Just year -> putStrLn $ "In 2020, you will be: " ++ show (2020 - year)
```

디커필링 코드
이 코드는 약간 커플된 코드입니다. 이 코드를 사용자에게 출력을 출력하는 함수와 나이를 계산하는 또다른 함수로 나누어 보겠습니다.

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided an invalid year"
        Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    let maybeAge =
            case readMay yearString of
                Nothing -> Nothing
                Just year -> Just (yearToAge year)
    displayAge maybeAge
```

This code does exactly the same thing as our previous version. But the definition of maybeAge in the main function looks pretty repetitive to me. We check if the parse year is Nothing. If it's Nothing, we return Nothing. If it's Just, we return Just, after applying the function yearToAge. That seems like a lot of line noise to do something simple. All we want is to conditionally apply yearToAge.
이 코드는 이전 버전과 정확히 같은것을 합니다. 그러나, 저에게 메인 함수에 있는 maybeAge 정의는 반복되고 있는것 같습니다. 우리는 파싱된 년도가 Nothing인지 확인합니다. 만약 Nothing이면, Nothing을 반환할것입니다. 만약 Just면, Just를 반환합니다.

펑터
당행히도, 우리는 그것을 하는 헬퍼 함수를 가지고 있습니다. fmap 또는 펑터 맵핑은 어떤 함수를 펑터가 가지고 있는 값에 적용할것입니다. Maybe는 펑터의 한 예입니다. 또다른 흔한 예는 리스트입니다. Maybe의 경우에도 fmap이 정확히 위에서 기술된대로 작동할것입니다. 그래서 다음과 같이 바꿀 수 있습니다.

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided an invalid year"
        Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    let maybeAge = fmap yearToAge (readMay yearString)
    displayAge maybeAge
```

Our code definitely got shorter, and hopefully a bit clearer as well. Now it's obvious that all we're doing is applying the yearToAge function over the contents of the Maybe value.

So what is a functor? It's some kind of container of values. In Maybe, our container holds zero or one values. With lists, we have a container for zero or more values. Some containers are even more exotic; the IO functor is actually providing an action to perform in order to retrieve a value. The only thing functors share is that they provide some fmap function which lets you modify their contents.

do-notation
We have another option as well: we can use do-notation. This is the same way we've been writing our main function in so far. That's because- as we mentioned in the previous paragraph- IO is a functor as well. Let's see how we can change our code to not use fmap:

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided an invalid year"
        Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    let maybeAge = do
            yearInteger <- readMay yearString
            return $ yearToAge yearInteger
    displayAge maybeAge
```

Inside the do-block, we have the slurp operator <-. This operator is special for do-notation, and is used to pull a value out of its wrapper (in this case, Maybe). Once we've extracted the value, we can manipulate it with normal functions, like yearToAge. When we complete our do-block, we have to return a value wrapped up in that container again. That's what the return function does.

do-notation isn't available for all Functors; it's a special feature reserved only for Monads. Monads are an extension of Functors that provide a little extra power. We're not really taking advantage of any of that extra power here; we'll need to make our program more complicated to demonstrate it.

Dealing with two variables
It's kind of limiting that we have a hard-coded year to compare against. Let's fix that by allowing the user to specify the "future year." We'll start off with a simple implementation using pattern matching and then move back to do notation.

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge =
            case readMay birthYearString of
                Nothing -> Nothing
                Just birthYear ->
                    case readMay futureYearString of
                        Nothing -> Nothing
                        Just futureYear -> Just (futureYear - birthYear)
    displayAge maybeAge
```

OK, it gets the job done... but it's very tedious. Fortunately, do-notation makes this kind of code really simple:

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            birthYear <- readMay birthYearString
            futureYear <- readMay futureYearString
            return $ yearDiff futureYear birthYear
    displayAge maybeAge
```

This is very convenient: we've now slurped our two values in our do-notation. If either parse returns Nothing, then the entire do-block will return Nothing. This demonstrates an important property about Maybe: it provides short circuiting.

Without resorting to other helper functions or pattern matching, there's no way to write this code using just fmap. So we've found an example of code that requires more power than Functors provide, and Monads provide that power.

Partial application
But maybe there's something else that provides enough power to write our two-variable code without the full power of Monad. To see what this might be, let's look more carefully at our types.

We're working with two values: readMay birthYearString and readMay
futureYearString. Both of these values have the type Maybe Integer. And we want to apply the function yearDiff, which has the type Integer -> Integer
-> Integer.

If we go back to trying to use fmap, we'll seemingly run into a bit of a problem. The type of fmap- specialized for Maybe and Integer- is (Integer -> a) -> Maybe Integer -> Maybe a. In other words, it takes a function that takes a single argument (an Integer) and returns a value of some type a, takes a second argument of a Maybe Integer, and gives back a value of type Maybe a. But our function- yearDiff- actually takes two arguments, not one. So fmap can't be used at all, right?

Not true actually. This is where one of Haskell's very powerful features comes into play. Any time we have a function of two arguments, we can also look at is as a function of one argument which returns a function. We can make this more clear with parentheses:

```haskell
yearDiff :: Integer -> Integer -> Integer
yearDiff :: Integer -> (Integer -> Integer)
So how does that help us? We can look at the fmap function as:

fmap :: (Integer -> (Integer -> Integer))
     -> Maybe Integer -> Maybe (Integer -> Integer)
Then when we apply fmap to yearDiff, we end up with:

fmap yearDiff :: Maybe Integer -> Maybe (Integer -> Integer)
```

That's pretty cool. We can apply this to our readMay futureYearString and end up with:

fmap yearDiff (readMay futureYearString) :: Maybe (Integer -> Integer)
That's certainly very interesting, but it doesn't help us. We need to somehow apply this value of type Maybe (Integer -> Integer) to our readMay
birthYearString of type Maybe Integer. We can do this with do-notation:

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            yearToAge <- fmap yearDiff (readMay futureYearString)
            birthYear <- readMay birthYearString
            return $ yearToAge birthYear
    displayAge maybeAge
```

We can even use fmap twice and avoid the second slurp:

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            yearToAge <- fmap yearDiff (readMay futureYearString)
            fmap yearToAge (readMay birthYearString)
    displayAge maybeAge
```

But we don't have a way to apply our Maybe (Integer -> Integer) function to our Maybe Integer directly.

Applicative functors
And now we get to our final concept: applicative functors. The idea is simple: we want to be able to apply a function which is inside a functor to a value inside a functor. The magic operator for this is <*>. Let's see how it works in our example:

```haskell
import Safe (readMay)
import Control.Applicative ((<*>))

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge =
            fmap yearDiff (readMay futureYearString)
                <*> readMay birthYearString
    displayAge maybeAge
```

In fact, the combination of fmap and <*> is so common that we have a special operator, <$>, which is a synonym for fmap. That means we can make our code just a little prettier:

```haskell
import Safe (readMay)
import Control.Applicative ((<$>), (<*>))

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
-- show
    let maybeAge = yearDiff
            <$> readMay futureYearString
            <*> readMay birthYearString
-- /show
    displayAge maybeAge
```

Notice the distinction between <$> and <*>. The former uses a function which is not wrapped in a functor, while the latter applies a function which is wrapped up.

So we don't need Monads?
So if we can do such great stuff with functors and applicative functors, why do we need monads at all? The terse answer is context sensitivity: with a monad, you can make decisions on which processing path to follow based on previous results. With applicative functors, you have to always apply the same functions.

Let's give a contrived example: if the future year is less than the birth year, we'll assume that the user just got confused and entered the values in reverse, so we'll automatically fix it by reversing the arguments to yearDiff. With do-notation and an if statement, it's easy:

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            futureYear <- readMay futureYearString
            birthYear <- readMay birthYearString
            return $
                if futureYear < birthYear
                    then yearDiff birthYear futureYear
                    else yearDiff futureYear birthYear
    displayAge maybeAge
```

Exercises
Implement fmap using <*> and return.

```haskell
import Control.Applicative ((<*>), Applicative)
import Prelude (return, Monad)
import qualified Prelude

fmap :: (Applicative m, Monad m) => (a -> b) -> (m a -> m b)
-- show
fmap ... ... = FIXME
-- /show

main =
    case fmap (Prelude.+ 1) (Prelude.Just 2) of
        Prelude.Just 3 -> Prelude.putStrLn "Good job!"
        _ -> Prelude.putStrLn "Try again"
import Control.Applicative ((<*>))

-- show
myFmap function wrappedValue = return function <*> wrappedValue

main = print $ myFmap (+ 1) $ Just 5
-- /show
How is return implemented for the Maybe monad? Try replacing return with its implementation in the code above.

-- show
returnMaybe = FIXME
-- /show

main
    | returnMaybe "Hello" == Just "Hello" = putStrLn "Correct!"
    | otherwise = putStrLn "Incorrect, please try again"
return is simply the Just constructor. This gets defined as:

instance Monad Maybe where
    return = Just
```

yearDiff is really just subtraction. Try to replace the calls to yearDiff with explicit usage of the - operator.

```haskell
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            futureYear <- readMay futureYearString
            birthYear <- readMay birthYearString
            return $
-- show
                if futureYear < birthYear
                    then yearDiff birthYear futureYear
                    else yearDiff futureYear birthYear
-- /show
    displayAge maybeAge
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            futureYear <- readMay futureYearString
            birthYear <- readMay birthYearString
            return $
-- show
                if futureYear < birthYear
                    then birthYear - futureYear
                    else futureYear - birthYear
-- /show
    displayAge maybeAge
```

It's possible to write an applicative functor version of the auto-reverse-arguments code by modifying the yearDiff function. Try to do so.

```haskell
import Safe (readMay)
import Control.Applicative ((<$>), (<*>))

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

-- show
yearDiff futureYear birthYear = FIXME
-- /show

main
    | yearDiff 5 6 == 1 = putStrLn "Correct!"
    | otherwise = putStrLn "Please try again"
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

-- show
yearDiff futureYear birthYear
    | futureYear > birthYear = futureYear - birthYear
    | otherwise = birthYear - futureYear
-- /show

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            futureYear <- readMay futureYearString
            birthYear <- readMay birthYearString
            return $
                if futureYear < birthYear
                    then yearDiff birthYear futureYear
                    else yearDiff futureYear birthYear
    displayAge maybeAge
```

Now try to do it without modifying yearDiff directly, but by using a helper function which is applied to yearDiff.

```haskell
import Safe (readMay)
import Control.Applicative ((<$>), (<*>))

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear
-- show
yourHelperFunction f ...
-- /show

main
    | yourHelperFunction yearDiff 5 6 == 1 = putStrLn "Correct!"
    | otherwise = putStrLn "Please try again"
import Safe (readMay)

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            futureYear <- readMay futureYearString
            birthYear <- readMay birthYearString
            return $
                if futureYear < birthYear
                    then yourHelperFunction yearDiff birthYear futureYear
                    else yourHelperFunction yearDiff futureYear birthYear
    displayAge maybeAge

-- show
yourHelperFunction f x y
    | x > y = f x y
    | otherwise = f y x
-- /show
```
