

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgeqp3 = require( './zgeqp3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeqp3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeqp3;
