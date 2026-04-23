
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlar1v = require( './zlar1v.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlar1v, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlar1v;
