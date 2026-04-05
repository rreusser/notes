/*
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import zlartg = require( './index' );


// TESTS //

// The function returns a void...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectType void
}

// The compiler throws an error if the function is provided a first argument which is not a Complex128Array...
{
	zlartg( '10', 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( 10, 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( true, 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( false, 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( null, 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( undefined, 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( [], 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( {}, 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	zlartg( new Float64Array( 50 ), '10', new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), true, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), false, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), null, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), undefined, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), [], new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), {}, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a Complex128Array...
{
	zlartg( new Float64Array( 50 ), 10, '10', 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, 10, 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, true, 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, false, 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, null, 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, undefined, 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, [], 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, {}, 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), '10', new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), true, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), false, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), null, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), undefined, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), [], new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), {}, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a Float64Array...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, '10', 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, 10, 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, true, 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, false, 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, null, 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, undefined, 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, [], 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, {}, 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a number...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), '10', new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), true, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), false, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), null, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), undefined, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), [], new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), {}, new Float64Array( 50 ), 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a Complex128Array...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, '10', 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, 10, 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, true, 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, false, 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, null, 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, undefined, 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, [], 10, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, {}, 10, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), '10', new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), true, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), false, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), null, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), undefined, new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), [], new Float64Array( 50 ), 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), {}, new Float64Array( 50 ), 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a Complex128Array...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, '10', 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, 10, 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, true, 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, false, 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, null, 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, undefined, 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, [], 10 ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a tenth argument which is not a number...
{
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), '10' ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), true ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), false ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), null ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), undefined ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), [] ); // $ExpectError
	zlartg( new Float64Array( 50 ), 10, new Float64Array( 50 ), 10, new Float64Array( 25 ), 10, new Float64Array( 50 ), 10, new Float64Array( 50 ), {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zlartg(); // $ExpectError
	zlartg( new Float64Array( 50 ) ); // $ExpectError
}
