

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgebrd = require( './dgebrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgebrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgebrd;
