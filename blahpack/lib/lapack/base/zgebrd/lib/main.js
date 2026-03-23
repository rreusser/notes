

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgebrd = require( './zgebrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgebrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgebrd;
