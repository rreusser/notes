
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlangb = require( './zlangb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlangb, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlangb;
