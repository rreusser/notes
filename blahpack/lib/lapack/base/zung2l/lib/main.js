

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zung2l = require( './zung2l.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zung2l, 'ndarray', ndarray );


// EXPORTS //

module.exports = zung2l;
