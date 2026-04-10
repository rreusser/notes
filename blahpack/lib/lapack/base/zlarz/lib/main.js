
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarz = require( './zlarz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarz, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarz;
