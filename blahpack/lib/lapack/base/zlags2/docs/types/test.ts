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

import zlags2 = require( './index' );


// TESTS //

// The function returns a void...
{
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectType void
}

// The compiler throws an error if the function is provided a first argument which is not a boolean...
{
	zlags2( '10', 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( 10, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( null, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( undefined, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( [], 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( {}, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	zlags2( true, '10', 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, true, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, false, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, null, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, undefined, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, [], 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, {}, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	zlags2( true, 10, 1.0, '10', 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, true, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, false, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, null, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, undefined, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, [], 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, {}, 10, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	zlags2( true, 10, 1.0, 10, '10', 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, true, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, false, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, null, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, undefined, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, [], 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, {}, 1.0, 10, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	zlags2( true, 10, 1.0, 10, 10, 1.0, '10', 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, true, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, false, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, null, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, undefined, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, [], 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, {}, 10, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, '10', 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, true, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, false, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, null, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, undefined, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, [], 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, {}, 1.0, 10, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a tenth argument which is not a number...
{
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, '10', 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, true, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, false, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, null, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, undefined, 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, [], 1.0, 10, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, {}, 1.0, 10, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided a twelfth argument which is not a number...
{
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, '10', 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, true, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, false, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, null, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, undefined, 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, [], 1.0 ); // $ExpectError
	zlags2( true, 10, 1.0, 10, 10, 1.0, 10, 10, 1.0, 10, 1.0, {}, 1.0 ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zlags2(); // $ExpectError
	zlags2( true ); // $ExpectError
}
