
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarzb = require( './zlarzb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarzb, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarzb;
