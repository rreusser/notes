

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zhemv = require( './zhemv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhemv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhemv;
