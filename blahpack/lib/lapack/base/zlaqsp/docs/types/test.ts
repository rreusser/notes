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

import zlaqsp = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	zlaqsp( 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( true, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( false, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( null, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( undefined, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( [], 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( {}, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	zlaqsp( 'no-transpose', '10', new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', true, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', false, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', null, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', undefined, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', [], new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', {}, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a Float64Array...
{
	zlaqsp( 'no-transpose', 10, '10', 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, true, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, false, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, null, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, undefined, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, [], 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, {}, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), '10', 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), true, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), false, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), null, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), undefined, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), [], 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), {}, 10, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, '10', new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, true, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, false, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, null, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, undefined, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, [], new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, {}, new Float64Array( 25 ), 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a Float64Array...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, '10', 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, true, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, false, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, null, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, undefined, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, [], 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, {}, 10, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), '10', 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), true, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), false, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), null, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), undefined, 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), [], 10, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), {}, 10, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, '10', 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, true, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, false, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, null, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, undefined, 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, [], 10, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, {}, 10, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a number...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, '10', 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, true, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, false, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, null, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, undefined, 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, [], 10, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, {}, 10, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a tenth argument which is not a number...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, '10', 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, true, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, false, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, null, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, undefined, 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, [], 'no-transpose' ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, {}, 'no-transpose' ); // $ExpectError
}

// The compiler throws an error if the function is provided a eleventh argument which is not a string...
{
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10 ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, true ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, false ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, null ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, undefined ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, [] ); // $ExpectError
	zlaqsp( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zlaqsp(); // $ExpectError
	zlaqsp( 'no-transpose' ); // $ExpectError
}
